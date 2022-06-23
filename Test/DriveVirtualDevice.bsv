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
import AXI4_Master_Socket::*;
import SourceSink::*;
import StmtFSM::*;
import Connectable::*;

(*synthesize*)
module mkDriveVirtualDevice();
  VirtualDeviceIfc#(2,32,64) vd <- mkVirtualDevice;
  AXI4_Master#(2, 32, 64, 0, 0, 0, 0, 0) axi_virt_sock <- mkAXI4_Master_Socket(10000);
  AXI4_Master#(2, 32, 64, 0, 0, 0, 0, 0) axi_mngt_sock <- mkAXI4_Master_Socket(10001);
  mkConnection(vd.virt, axi_virt_sock);
  mkConnection(vd.mngt, axi_mngt_sock);
endmodule
