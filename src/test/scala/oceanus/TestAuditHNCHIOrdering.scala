package oceanus

// Host-only protocol regressions: exercise the fail-fast checker independently
// of scheduler predicates, including multiple snoops and early CompAck.
object TestAuditHNCHIOrdering extends App {
  val rn=BigInt(8);val other=BigInt(9);val hn=BigInt(0);val pa=BigInt(0x206980)
  def rejects(body: =>Unit):Unit={
    var rejected=false
    try body catch {case e:IllegalStateException=>
      assert(e.getMessage.startsWith("HN_CHI_ORDER_VIOLATION"));rejected=true}
    assert(rejected,"illegal stimulus did not stop the checker")
  }
  val pending=new AuditHNCHIOrdering
  pending.request(rn,0,pa,true)
  pending.snoop(rn,hn,1,pa,37);pending.snoop(rn,hn,2,pa+32,38)
  assert(!pending.canData(rn,pa+63))
  assert(pending.canData(other,pa)&&pending.canData(rn,pa+64))
  rejects(pending.data(rn,0,0,42))
  pending.response(rn,hn,1,1,45)
  assert(!pending.canData(rn,pa))
  pending.response(rn,hn,2,1,46)
  pending.data(rn,0,0,47);pending.data(rn,0,2,48)
  assert(!pending.canSnoop(rn,pa)) // live/data queues can already be empty
  pending.response(rn,hn,0,2,49)
  assert(pending.canSnoop(rn,pa)&&pending.empty)

  val completion=new AuditHNCHIOrdering
  completion.request(rn,3,pa,true)
  completion.data(rn,3,2,50) // beat 2 may arrive first
  assert(completion.canSnoop(other,pa)&&completion.canSnoop(rn,pa+64))
  rejects(completion.snoop(rn,hn,4,pa,51))
  completion.response(rn,hn,3,2,52) // early acknowledgement
  assert(!completion.canSnoop(rn,pa)) // finish the multi-beat message
  completion.data(rn,3,0,53)
  completion.snoop(rn,hn,4,pa,54)
  completion.response(rn,hn,4,1,55)
  assert(completion.empty)

  val noAck=new AuditHNCHIOrdering
  noAck.request(rn,5,pa,false)
  noAck.data(rn,5,0,60);noAck.data(rn,5,2,61)
  assert(noAck.canSnoop(rn,pa)&&noAck.empty)

  val copy=new AuditHNCHIOrdering
  copy.eviction(rn,7,pa)
  copy.snoop(rn,hn,8,pa,70)
  rejects(copy.completion(rn,7,7,5,71))
  copy.response(rn,hn,8,1,72)
  copy.completion(rn,7,7,5,73)
  rejects(copy.snoop(rn,hn,9,pa,74))
  copy.writeData(rn,7,0,75)
  assert(!copy.canSnoop(rn,pa))
  copy.writeData(rn,7,2,76)
  assert(copy.canSnoop(rn,pa)&&copy.empty)
  copy.eviction(rn,10,pa);copy.completion(rn,10,10,4,80)
  assert(copy.empty)
  val unique=new AuditHNCHIOrdering
  unique.requestNoData(rn,11,pa,true)
  rejects(unique.response(rn,hn,11,2,81))
  rejects(unique.requestNoData(rn,11,pa,true))
  rejects(unique.request(rn,11,pa,true))
  rejects(unique.eviction(rn,11,pa))
  rejects(unique.data(rn,11,0,82))
  rejects(unique.completion(rn,11,11,5,83))
  unique.snoop(rn,hn,12,pa,84)
  rejects(unique.completion(rn,11,11,4,85))
  unique.response(rn,hn,12,1,86)
  unique.completion(rn,11,11,4,87)
  assert(!unique.canSnoop(rn,pa) && unique.canSnoop(other,pa))
  rejects(unique.completion(rn,11,11,4,88))
  rejects(unique.snoop(rn,hn,13,pa,89))
  unique.response(rn,hn,11,2,90)
  assert(unique.canSnoop(rn,pa) && unique.empty)
  rejects(unique.response(rn,hn,11,2,91))
  unique.requestNoData(rn,14,pa,false)
  unique.completion(rn,14,14,4,92)
  assert(unique.empty)
  println("HN_CHI_ORDER_UNIT pass=true pendingSnoop=true completionAck=true requesterLineIsolation=true earlyAck=true noExpCompAck=true copyBackCompletion=true copyBackWriteData=true makeUniqueComp=true makeUniqueCompAck=true makeUniqueNegative=true")
}
