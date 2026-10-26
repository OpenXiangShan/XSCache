package oceanus

import oceanus.compactchi.CCHIOpcode

/** Guards must fail before touching a DUT or partially queuing a batch. */
object TestType4UpstreamLegality extends App {
  def req(master: MasterId, opcode: BigInt) =
    UpRequestSpec(master, 0, 4, 0, opcode, BigInt("20100", 16))
  val illegal = req(MasterId.t4p0, CCHIOpcode.ReadShared.opcode)
  if (args.contains("--fail-upstream")) {
    new L2TopUpstreamModel(null, null).driveRead(illegal)
    sys.error("illegal Type-4 request reached DUT access")
  } else if (args.contains("--fail-driver")) {
    new L2TopConcurrentDriver(null, null).enqueue(Seq(illegal))
    sys.error("illegal Type-4 request entered driver queue")
  } else {
    var rejected = 0
    for (master <- Seq(MasterId.t4p0, MasterId.t4p1); opcode <- -1 to 64) {
      val request = req(master, opcode)
      val driver = new L2TopConcurrentDriver(null, null)
      if (opcode >= 0 && opcode <= 3) {
        L2TopUpstreamLegality.check(request)
        driver.enqueue(Seq(request))
        assert(driver.queued == 1)
      } else {
        def expectViolation(action: => Unit): Unit = {
          val error = try { action; None } catch { case e: IllegalArgumentException => Some(e) }
          assert(error.exists(_.getMessage.contains("CCHI_TYPE4_OPCODE_VIOLATION")))
          rejected += 1
        }
        expectViolation(L2TopUpstreamLegality.check(request))
        expectViolation(new L2TopUpstreamModel(null, null).driveRead(request))
        expectViolation(driver.enqueue(Seq(req(master, 3), request)))
        assert(driver.queued == 0, "invalid batch partially enqueued")
      }
    }
    Seq(MasterId.t1p0, MasterId.t1p1).foreach(m =>
      L2TopUpstreamLegality.check(req(m, CCHIOpcode.ReadShared.opcode)))
    println(s"TYPE4_LEGALITY_RESULT pass=true permitted=8 rejected=$rejected type1ReadShared=2")
  }
}
