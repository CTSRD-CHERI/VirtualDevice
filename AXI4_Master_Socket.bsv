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

import AXI4::*;
import AXI4_Utils::*;
import AXI4_Unified_Types::*;
import Socket::*;

module mkAXI4_Master_Socket(AXI4_Master#(i, a, d, 0, 0, 0, 0, 0));
  AXI4_Shim#(i, a, d, 0, 0, 0, 0, 0) axi <- mkAXI4ShimUGFF;
  Socket#(TDiv(SizeOf(ReqFlit#(i,a,d),8)),TDiv(SizeOf(RspFlit#(i,a,d),8)) sock <- mkSocket("virt_ifc", 12345);
  
  rule passRequest;
    Maybe#(Vector(TDiv(SizeOf(ReqFlit#(i,a,d),8)), Bit#(8))) mreqv <- sock.get();
    if (mreqv matches tagged Valid .reqv);
      ReqFlit#(i, d) req = unpack(truncate(pack(reqv)));
      enqReq(axi.slave,req);
    endrule
  endrule

  rule passResponse;
    Vector(TDiv(SizeOf(RspFlit#(i,a,d),8)), Bit#(8)) rspv = unpack(zeroExtend(pack(nextRsp(sock.slave))));
    sock.put(rspv);
    dropRsp(axi.slave);
  endrule
  
  return axi.master;
endmodule
