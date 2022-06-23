#-
# Copyright (c) 2022 Jonathan Woodruff
# All rights reserved.
#
# @BERI_LICENSE_HEADER_START@
#
# Licensed to BERI Open Systems C.I.C. (BERI) under one or more contributor
# license agreements.  See the NOTICE file distributed with this work for
# additional information regarding copyright ownership.  BERI licenses this
# file to you under the BERI Hardware-Software License, Version 1.0 (the
# "License"); you may not use this file except in compliance with the
# License.  You may obtain a copy of the License at:
#
#   http://www.beri-open-systems.org/legal/license-1-0.txt
#
# Unless required by applicable law or agreed to in writing, Work distributed
# under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR
# CONDITIONS OF ANY KIND, either express or implied.  See the License for the
# specific language governing permissions and limitations under the License.
#
# @BERI_LICENSE_HEADER_END@
#

BSC = bsc

BLUEUTILSDIR = ./BlueStuff
BSVPATH = +:BlueStuff:Test

# generated files directories
BUILDDIR = build
BDIR = $(BUILDDIR)/bdir
SIMDIR = $(BUILDDIR)/simdir

OUTPUTDIR = output
TOPMODULE = mkVirtualDevice
TESTMODULE = mkTestVirtualDevice
DRIVEMODULE = mkDriveVirtualDevice

BSCFLAGS += -bdir $(BDIR)
BSCFLAGS += -simdir $(SIMDIR)

BSCFLAGS += -show-schedule
BSCFLAGS += -sched-dot
BSCFLAGS += -show-range-conflict
#BSCFLAGS += -show-rule-rel \* \*
#BSCFLAGS += -steps-warn-interval n
LIBDIRS = %:BlueStuff:BlueStuff/BlueBasics:BlueStuff/BlueUtils:BlueStuff/AXI:BSV-RVFI-DII/:+

# Bluespec is not compatible with gcc > 4.9
# This is actually problematic when using $test$plusargs
#CC = gcc-4.8
#CXX = g++-4.8

TESTSDIR = Test
SIMTESTSSRC = $(sort $(wildcard $(TESTSDIR)/*.bsv))
SIMTESTS = $(addprefix sim, $(notdir $(basename $(SIMTESTSSRC))))

all: simTest simDrive verilog

simTest: $(TESTSDIR)/TestVirtualDevice.bsv VirtualDevice.bsv
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR) $(SIMDIR)
	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -simdir $(SIMDIR) $(BSCFLAGS) -p $(LIBDIRS) -sim -g $(TESTMODULE) -u $<
	CC=$(CC) CXX=$(CXX) $(BSC) -simdir $(SIMDIR) $(BSCFLAGS) -p $(LIBDIRS) -sim -o $(OUTPUTDIR)/$@ -e $(TESTMODULE)

simDrive: $(TESTSDIR)/DriveVirtualDevice.bsv VirtualDevice.bsv BSV-RVFI-DII/SocketPacketUtils/socket_packet_utils.c
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR) $(SIMDIR)
	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -simdir $(SIMDIR) $(BSCFLAGS) -p $(LIBDIRS) -sim -g $(DRIVEMODULE) -u $<
	CC=$(CC) CXX=$(CXX) $(BSC) -simdir $(SIMDIR) $(BSCFLAGS) -p $(LIBDIRS) -sim -o $(OUTPUTDIR)/$@ -e $(DRIVEMODULE) BSV-RVFI-DII/SocketPacketUtils/socket_packet_utils.c

verilog: VirtualDevice.bsv
	mkdir -p $(OUTPUTDIR)/$@-info $(BDIR)
	$(BSC) -info-dir $(OUTPUTDIR)/$@-info -vdir $(OUTPUTDIR) -opt-undetermined-vals -unspecified-to X $(BSCFLAGS) -p $(LIBDIRS) -verilog -g VirtualDevice.v -u $<

.PHONY: clean mrproper all

clean:
	rm -f .simTests
	rm -f -r $(BUILDDIR)

mrproper: clean
	rm -f -r $(OUTPUTDIR)
