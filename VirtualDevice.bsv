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
import AXI4_Unified_Types::*;

/**
 * An implementation of a peripheral that facilitates emulation of a virtual
 * peripheral by a core. A large window is exposed to the "duped" core to which
 * it may read and write.  All reads and writes are exposed via fifo-like
 * interfaces to the "colluding" core.
 *
 */
interface VirtualDeviceIfc#(
  numeric type im,
  numeric type am,
  numeric type dm,
  numeric type iv,
  numeric type av,
  numeric type dv);
  interface AXI4_Slave#(im, am, dm, 0, 0, 0, 0, 0) mngt;
  interface AXI4_Slave#(iv, av, dv, 0, 0, 0, 0, 0) virt;
endinterface

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
function Bit#(11) byte2regAddr(Bit#(14) a) = truncate(a>>3);
typedef struct {
   Bool isRead;
   Bit#(TAdd#(wordaddress_width,2))  addr; // word address
   } Req#(numeric type wordaddress_width) deriving(Bits,Eq);

/**
 * Implementation of the VirtualDeviceIfc interface. This uses a 20-bit 
 * address window for the virtual device.
 */
//(*synthesize*)
module mkVirtualDevice (VirtualDeviceIfc#(im,am,dm,iv,av,dv))
  provisos (Add#(d_  , 32, dm),         // Management data needs to be bigger than 32 bits.
            Add#(d__ , 8, dm),
            Add#(d___, TDiv#(dv, 8), dm), // Need to be able to read the byte enables.
            Div#(dm, 8, dmBytes),       // Number of bytes the management interface can read.
            Mul#(TDiv#(dm, 8), 8, dm),  // dm is an even multiple of 8
            Add#(a___, 64, dv),         // dv is at least 64 (because I incedentally extend the widest control register into a 
            Div#(dv, 8, dvBytes),       // Number of bytes of data for the virtualised interface.
            Div#(dv, dm, dms_in_dv),    // Number of management data words that fit in a virtualised data word.
            Mul#(dms_in_dv, dm, dv),    // Say it twice!  It's more fun!
            Add#(aa_, 14, am),          // Address should be more than 14 bits.
            Add#(a__, TLog#(dms_in_dv), am), // Address size should be bigger than the difference between the two data words.
            Add#(ab_, av, dv)           // At some point we extend a Bit#(av) into a Bit#(dv), so this.
           );
  
  AXI4_Shim#(im, am, dm, 0, 0, 0, 0, 0) shimMngt <- mkAXI4ShimUGFF;
  let mngtAXI = shimMngt.master;
  AXI4_Shim#(iv, av, dv, 0, 0, 0, 0, 0) shimVirt <- mkAXI4ShimUGFF;
  let virtAXI = shimVirt.master;

  /* Request data. */
  FF#(RequestRecord#(iv,av,dv), ReqDepth) reqQue <- mkUGFF;
  Reg#(Bit#(dv)) readResponseReg <- mkConfigRegU;
  Reg#(Bit#(32)) timeReg <- mkConfigReg(0);
  Reg#(Bool) enabledReg <- mkConfigReg(False);
  Reg#(Bit#(16)) nextReqId <- mkConfigReg(0);
  FF#(RspFlit#(iv, dv),2) responses <- mkFF;

  Bool verbose = True;
  
  function Bit#(dat_narrow) select_dm_in_dv(Bit#(dat_wide) d, Bit#(ma) a)
    provisos (Add#(a__, TLog#(dms_in_dv), ma),
              Mul#(dms_in_dv, dat_narrow, dat_wide));
    Vector#(dms_in_dv,Bit#(dat_narrow)) dm_vec = unpack(d);
    Bit#(TLog#(dms_in_dv)) sel = truncate(a >> valueOf(TLog#(dmBytes)));
    return dm_vec[sel];
  endfunction
  
  function Bit#(dat_wide) insert_dm_into_dv(Bit#(dat_wide) d, Bit#(dat_narrow) d_ins, Bit#(ma) a)
    provisos (Add#(a__, TLog#(dms_in_dv), ma),
              Mul#(dms_in_dv, dat_narrow, dat_wide));
    Vector#(dms_in_dv,Bit#(dat_narrow)) dm_vec = unpack(d);
    Bit#(TLog#(dms_in_dv)) sel = truncate(a >> valueOf(TLog#(dmBytes)));
    dm_vec[sel] = d_ins;
    return pack(dm_vec);
  endfunction

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
      enqRsp(virtAXI, defaultRspFromReq(req,0));
      if (verbose) $display("<time %0t, virtDev> send device response when disabled ", $time, fshow(defaultRspFromReq(req,0)));
    end
  endrule: handleVirtRequest

  /* Handle reads/writes to/from the management register interface */
  rule handleMngtRequest(nextReq(mngtAXI) matches tagged Valid .req);
    dropReq(mngtAXI);
    if (verbose) $display("<time %0t, virtDev> handle management request ", $time, fshow(req));
    RspFlit#(im, dm) resp = defaultRspFromReq(req, ?);

    /* Handle register reads. */
    RequestRecord#(iv,av,dv) next = reqQue.first;
    Bit#(11) reg_addr = byte2regAddr(truncate(getAddr(req)));
    case (req) matches
      tagged Read .ar: begin
        AXI4_RFlit#(im,dm,0) r = defaultValue;
        r.rid = ar.arid;
        r.rdata = 0;
        case (reg_addr)
          /* 0x0000-0x0008 read address*/
          'h0: begin
            Bit#(dv) wide = zeroExtend(pack(getAddr(next.req)));
            r.rdata = select_dm_in_dv(wide, getAddr(req));
          end
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
          'h8: begin
              r.rdata = select_dm_in_dv(readResponseReg, getAddr(req));
          end
          /* 0x1000-0x1008 write address*/
          'h200: begin
            Bit#(dv) wide = zeroExtend(pack(getAddr(next.req)));
            r.rdata = select_dm_in_dv(wide, getAddr(req));
          end
          /* 0x1008-0x100C write byte enable */
          'h201: begin
            if (next.req matches tagged Write .wr)
              r.rdata = zeroExtend(wr.w.wstrb);
          end
          /* 0x1040-0x1060 write data*/
          'h208, 'h209, 'h20a, 'h20b: begin
            if (next.req matches tagged Write .wr)
              r.rdata = select_dm_in_dv(wr.w.wdata, getAddr(req));
          end
          /* 0x2000-0x2004 time stamp*/
          /* 0x2004-0x2008 request_level (0x2007), request_is_write (0x2006), request_id (0x2004-0x2005) */
          'h400: begin
            Byte level = fromInteger(valueOf(ReqDepth)) - zeroExtend(reqQue.remaining);
            Byte nextIsWrite = (!next.isRead) ? 1:0;
            Bit#(dv) wide = zeroExtend({level,nextIsWrite,next.requestId,next.timeStamp});
            r.rdata = select_dm_in_dv(wide, getAddr(req));
          end
          /* 0x2008-0x2009 enable_device_emulation */
          'h401: begin
            Bit#(32) wide = zeroExtend(pack(enabledReg));
            r.rdata = zeroExtend(wide);
          end
        endcase
        resp = tagged Read r;
      end
      tagged Write .wr: begin
        case (reg_addr)
          /* 0x0040-0x0080 readresponse_data, a read/write field.*/
          'h8, 'h9, 'ha, 'hb: begin
            Vector#(dmBytes, Byte) neu = unpack(wr.w.wdata);
            Vector#(dmBytes, Byte) old = unpack(select_dm_in_dv(readResponseReg, getAddr(req)));
            Vector#(dmBytes, Bool) stb = unpack(wr.w.wstrb);
            neu = unpack(pack(zipWith3(choose, stb, neu, old)));
            readResponseReg <= insert_dm_into_dv(readResponseReg, pack(neu), getAddr(req));
          end
          /* 0x2000-0x2008 write triggers response */
          'h400: begin
            if (reqQue.notEmpty) begin
              RspFlit#(iv, dv) virtResp = defaultRspFromReq(next.req, pack(readResponseReg));
              if (verbose) $display("<time %0t, virtDev> send device response ", $time, fshow(virtResp));
              enqRsp(virtAXI, virtResp);
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

    if (verbose) $display("<time %0t, virtDev> sending response", $time, fshow(resp));
    /* Enqueue the response to management interface. */
    enqRsp(mngtAXI, resp);
  endrule: handleMngtRequest

  /* Slave for management interface exposing the samples in registers. */
  interface mngt  = toGuarded_AXI4_Slave(shimMngt.slave);
  /* Slave interface exposing a large address space for virtualisation. */
  interface virt  = toGuarded_AXI4_Slave(shimVirt.slave);
endmodule: mkVirtualDevice

endpackage: VirtualDevice
