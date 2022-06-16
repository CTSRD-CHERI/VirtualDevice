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

package VirtualDevice;

import ConfigReg::*;
import FF::*;
import SourceSink::*;
import Vector::*;
import Routable :: *;
import AXI4::*;
import AXI4_Utils::*;
import DefaultValue::*;

/**
 * An implementation of a peripheral that facilitates emulation of a virtual
 * peripheral by a core. A large window is exposed to the "duped" core to which
 * it may read and write.  All reads and writes are exposed via fifo-like
 * interfaces to the "colluding" core.
 *
 */
interface VirtualDeviceIfc#(
  numeric type i,
  numeric type a,
  numeric type d);
  interface AXI4_Slave#(i, a, d, 0, 0, 0, 0, 0) mngt;
  interface AXI4_Slave#(i, a, d, 0, 0, 0, 0, 0) virt;
endinterface

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

function Action enqReq(AXI4_Master#(i, a, d, 0, 0, 0, 0, 0) axim, RspFlit#(i, d) rsp) = action
  case (rsp) matches
    tagged Read  .r: axim.r.put(r);
    tagged Write .w: axim.b.put(w);
  endcase
endaction;

/* Other convenience types and functions  */

typedef Bit#(8) Byte;
typedef struct {
  Bool              isRead;
  Bit#(32)          timeStamp;
  Bit#(16)          requestId;
  ReqFlit#(i, a, d) req;
} RequestRecord#(numeric type i, numeric type a, numeric type d) deriving (Bits);
typedef 64 ReqDepth;

function Byte choose(Bool sel, Byte a, Byte b) = (sel) ? a:b;
function Bit#(11) byte2wordAddr(Bit#(14) a) = truncate(a>>3);
function Bit#(14) word2byteAddr(Bit#(11) a) = zeroExtend(a)<<3;
typedef struct {
   Bool isRead;
   Bit#(TAdd#(wordaddress_width,2))  addr; // word address
   } Req#(numeric type wordaddress_width) deriving(Bits,Eq);

/**
 * Implementation of the VirtualDeviceIfc interface. This uses a 20-bit 
 * address window for the virtual device.
 */
//(*synthesize*)
module mkVirtualDevice (VirtualDeviceIfc#(i,a,d))
  provisos (Add#(0, 64, d),          // Data needs to be 64 bits (for now).
            //Add#(db_, TDiv#(d, 8), d),// Data should be bigger than itself divided by 8 (duh...)
            //Div#(d, 8, dBytes),       // Data should be a multiple of bytes
            Add#(aa_, 14, a),         // Address should be more than 14 bits.
            Add#(ab_, a, d)           // Data size should be greater than or equal to address size
            
           );
  AXI4_Shim#(i, a, d, 0, 0, 0, 0, 0) shimMngt <- mkAXI4ShimUGFF;
  let mngtAXI = shimMngt.master;
  AXI4_Shim#(i, a, d, 0, 0, 0, 0, 0) shimVirt <- mkAXI4ShimUGFF;
  let virtAXI = shimVirt.master;

  /* Request data. */
  FF#(RequestRecord#(i,a,d), ReqDepth) reqQue <- mkUGFF;
  Reg#(Bit#(d)) readResponseReg <- mkConfigRegU;
  Reg#(Bit#(32)) timeReg <- mkConfigReg(0);
  Reg#(Bool) enabledReg <- mkConfigReg(False);
  Reg#(Bit#(16)) nextReqId <- mkConfigReg(0);
  FF#(RspFlit#(i, d),2) responses <- mkFF;

  Bool verbose = True;

  rule countTime;
    timeReg <= timeReg + 1;
  endrule
  /* Handle reads/writes to/from the virtual device adapter
   * slave interface. */
  rule handleVirtRequest(nextReq(virtAXI) matches tagged Valid .req &&& reqQue.notFull);
    dropReq(virtAXI);
    if (verbose) $display("<time %0t, virtDev> handle device request ", $time, fshow(req));
    if (enabledReg) begin
      Bool isRead = True;
      if (req matches tagged Write .w) isRead = False;
      reqQue.enq(RequestRecord{isRead: isRead, timeStamp: timeReg, requestId: nextReqId, req: req});
      nextReqId <= nextReqId + 1;
    end

    /* Enqueue the response on a write.
       For a read, we wait for the register interface to send a response
       and reply in the feedReadResp rule. */
    if (!enabledReg) begin
      enqReq(virtAXI, defaultRspFromReq(req,0));
      if (verbose) $display("<time %0t, virtDev> send device response when disabled ", $time, fshow(defaultRspFromReq(req,0)));
    end
  endrule: handleVirtRequest

  /* Handle reads/writes to/from the management register interface */
  rule handleMngtRequest(nextReq(mngtAXI) matches tagged Valid .req);
    dropReq(mngtAXI);
    if (verbose) $display("<time %0t, virtDev> handle register request ", $time, fshow(req));
    RspFlit#(i, d) resp = defaultRspFromReq(req, ?);

    /* Handle register reads. */
    RequestRecord#(i,a,d) next = reqQue.first;
    Bit#(11) offset = byte2wordAddr(truncate(getAddr(req)));
    case (req) matches
      tagged Read .ar: begin
        AXI4_RFlit#(i,d,0) r = defaultValue;
        r.rid = ar.arid;
        r.rdata = 0;
        case (offset)
          /* 0x0000-0x0008 read address*/
          'h0: r.rdata = zeroExtend(pack(getAddr(next.req)));
          /* 0x0008-0x000C flit size: number of bytes to be accessed beyond the address. */
          'h1: begin
            if (next.req matches tagged Read .ar)
              r.rdata = fromInteger(bytesFromAXI4_Size(ar.arsize));
          end
          /* 0x000C-0x0010 burst count: number of flits expected in the response.  We do not currently support burst responses. */
          'h2: begin
            if (next.req matches tagged Read .ar)
              r.rdata = zeroExtend(ar.arlen);
          end
          /* 0x0040-0x0080 readresponse_data, a read/write field.*/
          'h8: r.rdata = readResponseReg;
          /* 0x1000-0x1008 write address*/
          'h200: r.rdata = zeroExtend(getAddr(next.req));
          /* 0x1008-0x100C write byte enable */
          'h201: begin
            if (next.req matches tagged Write .wr)
              r.rdata = zeroExtend(wr.w.wstrb);
          end
          /* 0x1040-0x1060 write data*/
          'h208: begin
            if (next.req matches tagged Write .wr)
              r.rdata = wr.w.wdata;
          end
          /* 0x2000-0x2004 time stamp*/
          /* 0x2004-0x2008 request_level (0x2007), request_is_write (0x2006), request_id (0x2004-0x2005) */
          'h400: begin
            Byte level = fromInteger(valueOf(ReqDepth)) - zeroExtend(reqQue.remaining);
            Byte nextIsWrite = (!next.isRead) ? 1:0;
            r.rdata = {level,nextIsWrite,next.requestId,next.timeStamp};
          end
          /* 0x2008-0x2009 enable_device_emulation */
          'h401: r.rdata = zeroExtend(pack(enabledReg));
        endcase
        resp = tagged Read r;
      end
      tagged Write .wr: begin
        case (offset)
          /* 0x0040-0x0080 readresponse_data, a read/write field.*/
          'h8: begin
            Vector#(TDiv#(d,8), Byte) neu = unpack(pack(wr.w.wdata));
            Vector#(TDiv#(d,8), Byte) old = unpack(readResponseReg);
            Vector#(TDiv#(d,8), Bool) stb = unpack(wr.w.wstrb);
            readResponseReg <= pack(zipWith3(choose, stb, neu, old));
          end
          /* 0x2000-0x2008 write triggers response */
          'h400: begin
            if (reqQue.notEmpty) begin
              RspFlit#(i, d) virtResp = defaultRspFromReq(next.req, readResponseReg);
              if (verbose) $display("<time %0t, virtDev> send device response ", $time, fshow(virtResp));
              enqReq(virtAXI, virtResp);
              reqQue.deq();
            end else if (verbose) $display("<time %0t, virtDev> attempted device response when reqQue empty", $time);
          end
          /* 0x2008-0x2009 enable_device_emulation */
          'h401: begin
            if (verbose) $display("<time %0t, virtDev> wrote enabled bit %d -> %d", $time, enabledReg, wr.w.wdata[0]);
            enabledReg <= unpack(wr.w.wdata[0]);
          end
        endcase
      end
    endcase

    /* Enqueue the response to management interface. */
    enqReq(mngtAXI, resp);
  endrule: handleMngtRequest

  /* Slave for management interface exposing the samples in registers. */
  interface mngt  = toGuarded_AXI4_Slave(shimMngt.slave);
  /* Slave interface exposing a large address space for virtualisation. */
  interface virt  = toGuarded_AXI4_Slave(shimVirt.slave);
endmodule: mkVirtualDevice

endpackage: VirtualDevice
