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

import AXI4::*;
//import AXI4_Utils::*;
import DefaultValue::*;
import Vector::*;

typedef Bit#(8) B1;
typedef Bit#(16) B2;
typedef Bit#(32) B4;
typedef Bit#(64) B8;

/* Byte-stream formats for AXI flits;
 * These useful for communicating over sockets, e.g. to software in simulation.*/

// AW - write address type

typedef struct {
  B2 awid;
  B8 awaddr;
  B1 awlen;
  B1 awsize;
  B1 awburst;
  B1 awlock;
  B1 awcache;
  B1 awprot;
  B1 awqos;
  B1 awregion;
  B1 awuser;
} AXI4_AWFlit_BS
deriving (Bits, FShow);

function AXI4_AWFlit_BS aw_to_bs(AXI4_AWFlit#(i,a,u) aw)
  provisos(Add#(c__, i, 16), Add#(b__, a, 64), Add#(a__, u, 8)) =
  AXI4_AWFlit_BS{
    awid: zeroExtend(aw.awid),
    awaddr: zeroExtend(aw.awaddr),
    awlen: zeroExtend(aw.awlen),
    awsize: zeroExtend(pack(aw.awsize)),
    awburst: zeroExtend(pack(aw.awburst)),
    awlock: zeroExtend(pack(aw.awlock)),
    awcache: zeroExtend(aw.awcache),
    awprot: zeroExtend(aw.awprot),
    awqos: zeroExtend(aw.awqos),
    awregion: zeroExtend(aw.awregion),
    awuser: zeroExtend(aw.awuser)
  };

function AXI4_AWFlit#(i,a,u) bs_to_aw (AXI4_AWFlit_BS aw)
  provisos(Add#(c__, i, 16), Add#(b__, a, 64), Add#(a__, u, 8)) =
  AXI4_AWFlit{
    awid: truncate(aw.awid),
    awaddr: truncate(aw.awaddr),
    awlen: truncate(aw.awlen),
    awsize: unpack(truncate(aw.awsize)),
    awburst: unpack(truncate(aw.awburst)),
    awlock: unpack(truncate(aw.awlock)),
    awcache: truncate(aw.awcache),
    awprot: truncate(aw.awprot),
    awqos: truncate(aw.awqos),
    awregion: truncate(aw.awregion),
    awuser: truncate(aw.awuser)
  };

// W - write type

typedef struct {
  B8 wdata;
  B1 wstrb;
  B1 wlast;
  B1 wuser;
} AXI4_WFlit_BS deriving (Bits, FShow);

function AXI4_WFlit_BS w_to_bs(AXI4_WFlit#(d,u) w)
  provisos(Add#(b__, d, 64), Add#(a__, u, 8), Add#(c__, TDiv#(d, 8), 8)) =
  AXI4_WFlit_BS{
    wdata: zeroExtend(w.wdata),
    wstrb: zeroExtend(w.wstrb),
    wlast: zeroExtend(pack(w.wlast)),
    wuser: zeroExtend(w.wuser)
  };

function AXI4_WFlit#(d,u) bs_to_w(AXI4_WFlit_BS w)
  provisos(Add#(b__, d, 64), Add#(a__, u, 8), Add#(c__, TDiv#(d, 8), 8)) =
  AXI4_WFlit{
    wdata: truncate(w.wdata),
    wstrb: truncate(w.wstrb),
    wlast: unpack(truncate(w.wlast)),
    wuser: truncate(w.wuser)
  };

// B - write response type

typedef struct {
  B2 bid;
  B1 bresp;
  B1 buser;
} AXI4_BFlit_BS deriving (Bits, FShow);

function AXI4_BFlit_BS b_to_bs(AXI4_BFlit#(i,u) b)
  provisos(Add#(a__, u, 8), Add#(b__, i, 16)) =
  AXI4_BFlit_BS{
    bid: zeroExtend(b.bid),
    bresp: zeroExtend(pack(b.bresp)),
    buser: zeroExtend(b.buser)
  };

function AXI4_BFlit#(i,u) bs_to_b(AXI4_BFlit_BS b)
  provisos(Add#(a__, u, 8), Add#(b__, i, 16)) =
  AXI4_BFlit{
    bid: truncate(b.bid),
    bresp: unpack(truncate(b.bresp)),
    buser: truncate(b.buser)
  };
  
// AR - Read Address Request

typedef struct {
  B2 arid;
  B8 araddr;
  B1 arlen;
  B1 arsize;
  B1 arburst;
  B1 arlock;
  B1 arcache;
  B1 arprot;
  B1 arqos;
  B1 arregion;
  B1 aruser;
} AXI4_ARFlit_BS deriving (Bits, FShow);

function AXI4_ARFlit_BS ar_to_bs(AXI4_ARFlit#(i,a,u) ar)
  provisos(Add#(a__, i, 16), Add#(b__, a, 64), Add#(c__, u, 8)) =
  AXI4_ARFlit_BS{
    arid: zeroExtend(ar.arid),
    araddr: zeroExtend(ar.araddr),
    arlen: zeroExtend(ar.arlen),
    arsize: zeroExtend(pack(ar.arsize)),
    arburst: zeroExtend(pack(ar.arburst)),
    arlock: zeroExtend(pack(ar.arlock)),
    arcache: zeroExtend(ar.arcache),
    arprot: zeroExtend(ar.arprot),
    arqos: zeroExtend(ar.arqos),
    arregion: zeroExtend(ar.arregion),
    aruser: zeroExtend(ar.aruser)
  };

function AXI4_ARFlit#(i,a,u) bs_to_ar(AXI4_ARFlit_BS ar)
  provisos(Add#(a__, i, 16), Add#(b__, a, 64), Add#(c__, u, 8)) =
  AXI4_ARFlit{
    arid: truncate(ar.arid),
    araddr: truncate(ar.araddr),
    arlen: truncate(ar.arlen),
    arsize: unpack(truncate(ar.arsize)),
    arburst: unpack(truncate(ar.arburst)),
    arlock: unpack(truncate(ar.arlock)),
    arcache: truncate(ar.arcache),
    arprot: truncate(ar.arprot),
    arqos: truncate(ar.arqos),
    arregion: truncate(ar.arregion),
    aruser: truncate(ar.aruser)
  };

// R - Read Response

typedef struct {
  B2 rid;
  B8 rdata;
  B1 rresp;
  B1 rlast;
  B1 ruser;
} AXI4_RFlit_BS deriving (Bits, FShow);

function AXI4_RFlit_BS r_to_bs(AXI4_RFlit#(i,d,u) r)
  provisos(Add#(c__, i, 16), Add#(b__, d, 64), Add#(a__, u, 8)) =
  AXI4_RFlit_BS{
    rid: zeroExtend(r.rid),
    rdata: zeroExtend(r.rdata),
    rresp: zeroExtend(pack(r.rresp)),
    rlast: zeroExtend(pack(r.rlast)),
    ruser: zeroExtend(r.ruser)
  };

function AXI4_RFlit#(i,d,u) bs_to_r(AXI4_RFlit_BS r)
  provisos(Add#(c__, i, 16), Add#(b__, d, 64), Add#(a__, u, 8)) =
  AXI4_RFlit{
    rid: truncate(r.rid),
    rdata: truncate(r.rdata),
    rresp: unpack(truncate(r.rresp)),
    rlast: unpack(truncate(r.rlast)),
    ruser: truncate(r.ruser)
  };

// Unified bytestream flit type.
typedef enum {
  AW = 8'h0,
  W  = 8'h1,
  B  = 8'h2,
  AR = 8'h3,
  R  = 8'h4,
  Unused = 8'hFF
} AXI4_BS_FlitKind deriving (Bits, Eq, FShow);

typedef struct {
  AXI4_BS_FlitKind kind;
  Vector#(19, Bit#(8)) flit; // big enough for the biggest flit
} AXI4_Flit_BS deriving (Bits, FShow); // 20 bytes total
