trait Simulation {
  type Action = () => Unit
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = List()
  private var curtime = 0
  def currentTime: Int = curtime
  def afterDelay(delay: Int)(block: => Unit): Unit = {
    val event = Event(curtime + delay, () => block)
    agenda = insert(agenda, event)
  }
  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if (first.time < item.time) => first :: insert(rest, item)
    case _ => item :: ag
  }
  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }
  def run(): Unit = {
    afterDelay(0) {
      println(s"*** simulation started, time = '$currentTime ' ***")
    }
    loop()
  }
}

abstract class Gates extends Simulation {

  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int

  class Wire {
    private var sigVal = false
    private var actions: List[Action] = List()
    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit =
      if(s != sigVal) {
        sigVal = s
        actions foreach(_())
      }
    def addAction(a: Action): Unit = {
      actions = a :: actions
      a()
    }
  }

  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) {output setSignal(!inputSig)}
    }
    input addAction(invertAction)
  }

  def andGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(AndGateDelay) {output setSignal(in1Sig & in2Sig)}
    }
    in1 addAction (andAction)
    in2 addAction (andAction)
  }

  def orGate(in1: Wire, in2: Wire, output: Wire): Unit = {
    def orAction(): Unit = {
      val in1Sig = in1.getSignal
      val in2Sig = in2.getSignal
      afterDelay(OrGateDelay) {output setSignal(in1Sig | in2Sig)}
    }
    in1 addAction (orAction)
    in2 addAction (orAction)
  }

  def orGateAlt(in1: Wire, in2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(in1, notIn1); inverter(in2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }

  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime new-value = ${wire.getSignal}")
    }
    wire addAction(probeAction)
  }
}

abstract class Circuits extends Gates {
  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire): Unit = {
    val d = new Wire
    val e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire): Unit = {
    val s = new Wire
    val c1 = new Wire
    val c2 = new Wire
    halfAdder(b, cin, s, c1)
    halfAdder(a, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

trait Parameters {
  def InverterDelay = 2
  def AndGateDelay = 3
  def OrGateDelay = 5
}

object sim extends Circuits with Parameters
import sim._

val in1 = new Wire
val in2 = new Wire
val sum = new Wire
val curry = new Wire

halfAdder(in1, in2, sum, curry)
probe("sum", sum)
probe("curry", curry)

in1 setSignal(true)
run()
in2 setSignal(true)
run()
in1 setSignal(false)
run()
in2 setSignal(false)
run()
in1 setSignal(true)
run()

