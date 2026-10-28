package oceanus

import scala.collection.mutable

// Actual interface events, 64B lines, inclusive [cycle-511,cycle] window.
// Count each false->true interval once; cycles are line-cycles, not transactions.
class StressHNCDPOverlap {
  private val names=Seq("cdpEmitted","cdpIssued","dctForwardComplete","requesterDCT","splitRead","normalSnoop","stashSnoop")
  private val events=mutable.ArrayBuffer.empty[(Int,BigInt,Int)]
  private val last=mutable.Map.empty[(BigInt,Int),Int]
  private val expire=mutable.Queue.empty[(Int,BigInt,Int)]
  private val changed=mutable.Set.empty[BigInt]
  private val active=mutable.Map.empty[BigInt,Int].withDefaultValue(0)
  private val open=mutable.Map.empty[(BigInt,Int),Int]
  private val windows=Array.fill(128)(0L)
  private val cycles=Array.fill(128)(0L)
  private val masks=(1 to 127).filter(m=>Set(1,2)(m&3)&&(m&124)!=0)
  private val digest=java.security.MessageDigest.getInstance("SHA-256")
  def event(c:Int,pa:BigInt,name:String):Unit={
    val i=names.indexOf(name);require(i>=0)
    val line=pa>>6;events+=((c,line,i));last((line,i))=c;expire.enqueue((c+512,line,i));changed+=line
    digest.update(s"$c:${line.toString(16)}:$i\n".getBytes(java.nio.charset.StandardCharsets.UTF_8))
  }
  def tick(c:Int):Unit={
    while(expire.headOption.exists(_._1<=c)) {
      val (e,line,i)=expire.dequeue()
      if(last.get((line,i)).contains(e-512)){last.remove((line,i));changed+=line}
    }
    changed.foreach{line=>
      val before=active(line);val after=names.indices.filter(i=>last.contains((line,i))).map(i=>1<<i).sum
      masks.foreach{m=>
        val was=(before&m)==m;val now=(after&m)==m;val key=(line,m)
        if(!was&&now){windows(m)+=1;open(key)=c}
        if(was&& !now)cycles(m)+=c-open.remove(key).get
      }
      if(after==0)active.remove(line) else active(line)=after
    }
    changed.clear()
  }
  def report(caseId:String,seed:Int,closed:Boolean,end:Int):Unit={
    val online=cycles.clone();open.foreach{case((_,m),start)=>online(m)+=end-start}
    val checks=Array.fill(128)(0L);val checkCycles=Array.fill(128)(0L)
    // Independent offline interval-boundary scan; no shared rolling-window state.
    events.groupBy(_._2).foreach{case(_,es)=>
      val boundaries=mutable.Map.empty[Int,Array[Int]]
      es.foreach{case(c,_,i)=>
        boundaries.getOrElseUpdate(c,Array.fill(7)(0))(i)+=1
        boundaries.getOrElseUpdate(math.min(c+512,end),Array.fill(7)(0))(i)-=1
      }
      val live=Array.fill(7)(0);var before=0;var prev=0
      boundaries.toSeq.sortBy(_._1).foreach{case(c,delta)=>
        masks.filter(m=>(before&m)==m).foreach(m=>checkCycles(m)+=c-prev)
        for(i<-0 until 7)live(i)+=delta(i)
        val after=(0 until 7).filter(live(_)>0).map(i=>1<<i).sum
        masks.filter(m=>(before&m)!=m&&(after&m)==m).foreach(m=>checks(m)+=1)
        before=after;prev=c
      }
    }
    val hash=digest.clone().asInstanceOf[java.security.MessageDigest].digest().map(b=>f"${b&255}%02x").mkString
    masks.foreach{m=>
      assert(windows(m)==checks(m)&&online(m)==checkCycles(m),s"HN/CDP overlap audit mask=$m end=$end")
      val label=names.indices.filter(i=>(m&(1<<i))!=0).map(names).mkString("+")
      println(s"STRESS_HN_OVERLAP caseId=$caseId seed=$seed closed=$closed combo=$label windows=${windows(m)} lineCycles=${online(m)} audit=true eventCount=${events.size} eventsSHA256=$hash windowCycles=512 end=$end")
    }
  }
}
