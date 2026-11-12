/** *************************************************************************************
  * Copyright (c) 2020-2021 Institute of Computing Technology, Chinese Academy of Sciences
  * Copyright (c) 2020-2021 Peng Cheng Laboratory
  *
  * XiangShan is licensed under Mulan PSL v2.
  * You may obtain a copy of Mulan PSL v2 at:
  * http://license.coscl.org.cn/MulanPSL2
  * THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND.
  * *************************************************************************************
  */

package xscache.coupledL2.prefetch

import org.chipsalliance.cde.config.Parameters

// A separate instance with its own teacher, student pool, RR, delay queue and request buffer.
// The original VBOP retains its original candidate list and never supplies state to UBOP.
class UBestOffsetPrefetch(implicit p: Parameters) extends VBestOffsetPrefetch("ubop")
