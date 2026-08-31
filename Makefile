ISSUE ?= B
NUM_CORE ?= 2
NUM_TL_UL ?= 0
NUM_SLICE ?= 4
WITH_CHISELDB ?= 1
WITH_TLLOG ?= 1
WITH_CHILOG ?= 1
BY_ETIME ?= 1
BY_VTIME ?= 0
FPGA ?= 0

init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i XSCache.compile

CHI_PASS_ARGS = ISSUE=$(ISSUE) NUM_CORE=$(NUM_CORE) NUM_TL_UL=$(NUM_TL_UL) NUM_SLICE=$(NUM_SLICE) \
			    WITH_CHISELDB=$(WITH_CHISELDB) WITH_TLLOG=$(WITH_TLLOG) WITH_CHILOG=$(WITH_CHILOG) \
				BY_ETIME=$(BY_ETIME) BY_VTIME=$(BY_VTIME) \
			    FPGA=$(FPGA)

TOP = TestTop
CHI_TOP_ARGS = --issue $(ISSUE) --core $(NUM_CORE) --tl-ul $(NUM_TL_UL) --bank $(NUM_SLICE) \
		   	   --chiseldb $(WITH_CHISELDB) --tllog $(WITH_TLLOG) --chilog $(WITH_CHILOG) \
			   --etime $(BY_ETIME) --vtime $(BY_VTIME) \
		       --fpga $(FPGA)
BUILD_DIR_L2 = ./build/coupledl2
BUILD_DIR_LLC = ./build/openllc
TOP_V_L2 = $(BUILD_DIR_L2)/$(TOP).sv
TOP_V_LLC = $(BUILD_DIR_LLC)/$(TOP).sv
MEM_GEN = ./scripts/vlsi_mem_gen
MEM_GEN_SEP = ./scripts/gen_sep_mem.sh

gen-test-top:
	mill -i XSCache.test.runMain xscache.coupledL2.$(TOP)_$(SYSTEM) -td $(BUILD_DIR_L2) --target systemverilog --split-verilog
	if [ -f "$(TOP_V_L2).conf" ]; then $(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V_L2).conf" "$(BUILD_DIR_L2)"; fi

gen-test-top-chi:
	mill -i XSCache.test.runMain xscache.coupledL2.$(TOP)_$(SYSTEM) -td $(BUILD_DIR_L2) $(CHI_TOP_ARGS) --target systemverilog --split-verilog
	if [ -f "$(TOP_V_L2).conf" ]; then $(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V_L2).conf" "$(BUILD_DIR_L2)"; fi

test-top-chi:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS)

test-top-chi-dualcore-0ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=2 NUM_TL_UL=0

test-top-chi-dualcore-2ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=2 NUM_TL_UL=2

test-top-chi-quadcore-0ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=4 NUM_TL_UL=0

test-top-chi-quadcore-2ul:
	$(MAKE) gen-test-top-chi SYSTEM=CHIL2 $(CHI_PASS_ARGS) NUM_CORE=4 NUM_TL_UL=2

test-top-l3-openllc:
	mill -i XSCache.test.runMain xscache.openLLC.TestTop_L3 -td build --target systemverilog --split-verilog

test-top-l2l3-openllc:
	mill -i XSCache.test.runMain xscache.openLLC.TestTopSoC_SingleCore -td $(BUILD_DIR_LLC) --target systemverilog --split-verilog

test-top-l2l3l2-openllc:
	mill -i XSCache.test.runMain xscache.openLLC.TestTopSoC_DualCore -td $(BUILD_DIR_LLC) --target systemverilog --split-verilog

BUILD_DIR_TSHRCTRL = ./build/l2tshrctrl
TOP_V_TSHRCTRL = $(BUILD_DIR_TSHRCTRL)/TestTop_L2TSHRCtrl.sv

test-top-tshrctrl:
	mill -i XSCache.test.runMain oceanus.TestTop_L2TSHRCtrl -td $(BUILD_DIR_TSHRCTRL) --target systemverilog --split-verilog
	if [ -f "$(TOP_V_TSHRCTRL).conf" ]; then $(MEM_GEN_SEP) "$(MEM_GEN)" "$(TOP_V_TSHRCTRL).conf" "$(BUILD_DIR_TSHRCTRL)"; fi

BUILD_DIR_L2TOP = ./build/l2top

# defaults to 2 slices; override with NUM_SLICE=<1-4>
test-top-l2top: NUM_SLICE = 2
test-top-l2top:
	mill -i XSCache.test.runMain oceanus.TestTop_L2Top -td $(BUILD_DIR_L2TOP) --slices $(NUM_SLICE) --target systemverilog --split-verilog
	if [ -f "$(BUILD_DIR_L2TOP)/TestTop.sv.conf" ]; then $(MEM_GEN_SEP) "$(MEM_GEN)" "$(BUILD_DIR_L2TOP)/TestTop.sv.conf" "$(BUILD_DIR_L2TOP)"; fi

BUILD_DIR_L2OPENLLC = ./build/l2openllc

# defaults to 1 L2 x 2 slices; override with NUM_L2=<n> / NUM_SLICE=<1-4>
NUM_L2 ?= 1
test-top-l2openllc: NUM_SLICE = 2
test-top-l2openllc:
	mill -i XSCache.test.runMain oceanus.TestTop_L2OpenLLC -td $(BUILD_DIR_L2OPENLLC) --l2 $(NUM_L2) --slices $(NUM_SLICE) --target systemverilog --split-verilog
	if [ -f "$(BUILD_DIR_L2OPENLLC)/TestTop.sv.conf" ]; then $(MEM_GEN_SEP) "$(MEM_GEN)" "$(BUILD_DIR_L2OPENLLC)/TestTop.sv.conf" "$(BUILD_DIR_L2OPENLLC)"; fi

clean:
	rm -rf ./build

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat

.PHONY: init bsp checkformat clean compile-coupledl2 compile-openllc idea reformat 
