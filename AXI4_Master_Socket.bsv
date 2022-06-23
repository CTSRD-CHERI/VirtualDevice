/*-
 * Copyright (c) 2022 Jonathan Woodruff
 * All rights reserved.
 *
 * @BERiLICENSE_HEADER_START@
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

import Vector::*;
import FIFO::*;
import SourceSink::*;
import Assert::*;
import AXI4::*;
import AXI4_Utils::*;
import AXI4_ByteStream_Types::*;
import Socket::*;

module mkAXI4_Master_Socket#(Integer tcp_port)(AXI4_Master#(i, a, d, 0, 0, 0, 0, 0))
  provisos(Add#(c__, i, 16), Add#(b__, a, 64), Add#(a__, d, 64), Add#(d__, TDiv#(d, 8), 8));
  AXI4_Shim#(i, a, d, 0, 0, 0, 0, 0) axi <- mkAXI4ShimUGFF;
  Socket#(20,20) sock <- mkSocket("virt_ifc", tcp_port);
/*
  function t unpackVector(Vector#(19, Bit#(8)) vec)
    provisos(Bits#(t, a__), Add#(a__, b__, 152)) =
    unpack(truncate(pack(vec)));
*/
  rule passRequest;
    Maybe#(Vector#(20, Bit#(8))) mreqv <- sock.get();
    if (mreqv matches tagged Valid .reqv) begin
      AXI4_Flit_BS req = unpack(pack(reqv));
      $display("Got AXI request! ", fshow(req));
      case(req.kind)
        AW: begin
          AXI4_AWFlit#(i,a,0) aw = bs_to_aw(unpack(truncate(pack(req.flit))));
          $display("Got AXI AW request! ", fshow(aw));
          axi.slave.aw.put(bs_to_aw(unpack(truncate(pack(req.flit)))));
        end
        W: axi.slave.w.put(bs_to_w(unpack(truncate(pack(req.flit)))));
        AR: axi.slave.ar.put(bs_to_ar(unpack(truncate(pack(req.flit)))));
        default: dynamicAssert(False, "Can only handle aw, w, ar over AXI request bytestream.");
      endcase
    end
  endrule

  rule passResponse;
    Maybe#(AXI4_Flit_BS) mreq = Invalid;
    if (axi.slave.b.canPeek) begin
      $display("Sending AXI response ", fshow(axi.slave.b.peek));
      mreq = Valid(AXI4_Flit_BS{kind: B, flit: unpack(zeroExtend(pack(b_to_bs(axi.slave.b.peek))))});
      axi.slave.b.drop;
    end else if (axi.slave.r.canPeek) begin
      $display("Sending AXI response ", fshow(axi.slave.r.peek));
      mreq = Valid(AXI4_Flit_BS{kind: R, flit: unpack(zeroExtend(pack(r_to_bs(axi.slave.r.peek))))});
      axi.slave.r.drop;
    end
    if (mreq matches tagged Valid .req) begin
      Bool better_work <- sock.put(unpack(pack(req)));
      dynamicAssert(better_work, "Putting to the socket failed.");
    end
  endrule

  return axi.master;
endmodule
