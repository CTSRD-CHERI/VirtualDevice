/*-
 * Copyright (c) 2022 Jonathan Woodruff
 * All rights reserved.
 *
 * @BERiLICENSE_HEADER_START@
 * This software was developed by SRI International and the University of
 * Cambridge Computer Laboratory (Department of Computer Science and
 * Technology) under DARPA contract HR0011-18-C-0016 ("ECATS"), as part of the
 * DARPA SSITH research programme.
 *
 * Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
 * license agreements.  See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.  BERI licenses this
 * file to you under the BERI Hardware-Software License, Version 1.0 (the
 * "License"); you may not use this file except in compliance with the
 * License.  You may obtain a copy of the License at:
 *
 *   http://www.beri-open-systems.org/legal/license-1-0.txt
 *
 * Unless required by applicable law or agreed to in writing, Work distributed
 * under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
 * CONDITIONS OF ANY KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations under the License.
 *
 * @BERiLICENSE_HEADER_END@
 */

import Routable :: *;
import AXI4::*;
import AXI4_Utils::*;
import SourceSink::*;
import DefaultValue::*;

/* Convenience functions to unify the AXI channels */

typedef struct {
  AXI4_AWFlit#(i,a,0) aw;
  AXI4_WFlit#(d,0) w;
} WriteReqFlit#(numeric type i, numeric type a, numeric type d)
deriving (Bits, FShow);

typedef union tagged {
  WriteReqFlit#(i,a,d) Write;
  AXI4_ARFlit#(i,a,0) Read;
} ReqFlit#(numeric type i, numeric type a, numeric type d)
deriving (Bits, FShow);
instance DefaultValue#(ReqFlit#(i, a, d));
  function defaultValue = tagged Read defaultValue;
endinstance

typedef union tagged {
  AXI4_BFlit#(i,0) Write;
  AXI4_RFlit#(i,d,0) Read;
} RspFlit#(numeric type i, numeric type d)
deriving (Bits, FShow);
instance DefaultValue#(RspFlit#(i,d));
  function defaultValue = tagged Write defaultValue;
endinstance

function RspFlit#(i,d) defaultRspFromReq(ReqFlit#(i,a,d) req, Bit#(d) data) =
  case (req) matches
    tagged Write .wr:
      return tagged Write AXI4_BFlit{ bid: wr.aw.awid
                                    , bresp: OKAY
                                    , buser: ? };
    tagged Read .ar: begin
      AXI4_RFlit#(i,d,0) r = defaultValue;
      r.rid = ar.arid;
      r.rdata = data;
      return tagged Read r;
    end
  endcase;

function Bit#(a) getAddr(ReqFlit#(i,a,d) req) =
  case (req) matches
    tagged Write .wr: return wr.aw.awaddr;
    tagged Read .ar: return ar.araddr;
  endcase;

function Maybe#(ReqFlit#(i, a, d)) nextReq(AXI4_Master#(i, a, d, 0, 0, 0, 0, 0) axim);
  if (axim.ar.canPeek) return Valid(Read(axim.ar.peek));
  else if (axim.w.canPeek) return
        Valid(Write(WriteReqFlit {
                aw: axim.aw.peek,
                w: axim.w.peek
        }));
  else return Invalid;
endfunction

function Action dropReq(AXI4_Master#(i, a, d, 0, 0, 0, 0, 0) axim) = action
  if (nextReq(axim) matches tagged Valid .r)
    case (r) matches
      tagged Read  .r: axim.ar.drop;
      tagged Write .w: begin
        if (isLast(axim.w.peek)) axim.aw.drop;
        axim.w.drop;
      end
    endcase
endaction;

function Maybe#(RspFlit#(i, d)) nextRsp(AXI4_Slave#(i, a, d, 0, 0, 0, 0, 0) axis);
  if (axis.r.canPeek) return Valid(Read(axis.r.peek));
  else if (axis.b.canPeek) return Valid(Write(axis.b.peek));
  else return Invalid;
endfunction

function Action dropRsp(AXI4_Slave#(i, a, d, 0, 0, 0, 0, 0) axis) = action
  if (nextRsp(axis) matches tagged Valid .rsp)
    case (rsp) matches
      tagged Read  .r: axis.r.drop;
      tagged Write .w: axis.b.drop;
    endcase
endaction;

function Action enqRsp(AXI4_Master#(i, a, d, 0, 0, 0, 0, 0) axim, RspFlit#(i, d) rsp) = action
  case (rsp) matches
    tagged Read  .r: axim.r.put(r);
    tagged Write .w: axim.b.put(w);
  endcase
endaction;

function Action enqReq(AXI4_Slave#(i, a, d, 0, 0, 0, 0, 0) axis, ReqFlit#(i, a, d) req) = action
  case (req) matches
    tagged Read  .ar: axis.ar.put(ar);
    tagged Write .w: begin
      axis.aw.put(w.aw); // doesn't handle bursts correctly here.
      axis.w.put(w.w);
    end
  endcase
endaction;
