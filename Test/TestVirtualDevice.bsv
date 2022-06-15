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
 
import VirtualDevice::*;
import AXI4::*;
import SourceSink::*;
import StmtFSM::*;
 
(*synthesize*)
module mkTestVirtualDevice();
  VirtualDeviceIfc#(2,32,64) vd <- mkVirtualDevice;
  Reg#(Bool) did_init <- mkReg(False);
  Stmt init_stmt = (seq
    action
      AXI4_AWFlit#(2,32,0) awFlit = defaultValue;
      awFlit.awaddr = 32'h2008;
      vd.mngt.aw.put(awFlit);
      AXI4_WFlit#(64,0) wFlit = defaultValue;
      wFlit.wdata = 1;
      wFlit.wstrb = 1;
      vd.mngt.w.put(wFlit);
    endaction
    while (!vd.mngt.b.canPeek) seq noAction; endseq
    vd.mngt.b.drop;
    did_init <= True;
  endseq);
  FSM init_fsm <- mkFSM(init_stmt);
  rule start_init(!did_init);
    init_fsm.start;
  endrule
  
  rule virtualRequest(did_init);
    AXI4_ARFlit#(2,32,0) readFlit = defaultValue;
    readFlit.araddr = 0;
    vd.virt.ar.put(readFlit);
    $display("Put one in virt: ", fshow(readFlit));
  endrule
  
  rule virtualResponse(vd.virt.r.canPeek);
    $display("Look at me! ", fshow(vd.virt.r.peek));
    vd.virt.r.drop;
  endrule
  
  Reg#(Bit#(32)) count <- mkReg(0);
  Reg#(Bool) serviceRequest <- mkReg(False);
  Stmt manage_stmt = (seq
    while (!serviceRequest) seq
      action
        AXI4_ARFlit#(2,32,0) arFlit = defaultValue;
        arFlit.araddr = 32'h2000;
        vd.mngt.ar.put(arFlit);
      endaction
      while (!vd.mngt.r.canPeek) seq noAction; endseq
      serviceRequest <= vd.mngt.r.peek.rdata[63:56]>0; // The level of the request FIFO is non-zero;
      $display("Management status register: %x", vd.mngt.r.peek.rdata);
      vd.mngt.r.drop;
    endseq
    action
      AXI4_AWFlit#(2,32,0) awFlit = defaultValue;
      awFlit.awaddr = 32'h0040;
      vd.mngt.aw.put(awFlit);
      AXI4_WFlit#(64,0) wFlit = defaultValue;
      wFlit.wdata = zeroExtend(count);
      count <= count + 1;
      wFlit.wstrb = 8'h0F;
      vd.mngt.w.put(wFlit);
    endaction
    action
      AXI4_AWFlit#(2,32,0) awFlit = defaultValue;
      awFlit.awaddr = 32'h2000;
      vd.mngt.aw.put(awFlit);
      AXI4_WFlit#(64,0) wFlit = defaultValue;
      wFlit.wdata = 1;
      wFlit.wstrb = 1;
      vd.mngt.w.put(wFlit);
    endaction
    while (!vd.mngt.b.canPeek) seq noAction; endseq
    vd.mngt.b.drop;
    while (!vd.mngt.b.canPeek) seq noAction; endseq
    vd.mngt.b.drop;
    serviceRequest <= False;
  endseq);
  FSM manage_fsm <- mkFSM(manage_stmt);
  rule start_manage(did_init);
    manage_fsm.start;
  endrule
endmodule
