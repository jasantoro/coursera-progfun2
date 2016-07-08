class Wire(var value: Boolean = false)
class Gate

def inverter(input: Wire, output: Wire): Unit = output.value = !input.value
def andGate(a1: Wire, a2: Wire, output: Wire): Unit = output.value = a1.value && a2.value
def orGate(o1: Wire, o2: Wire, output: Wire): Unit = output.value = o1.value || o2.value

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

def f(a: Wire, b: Wire, c: Wire): Unit = {
  val d = new Wire
  val e = new Wire
  val f = new Wire
  val g = new Wire
  inverter(a, d)
  inverter(b, e)
  andGate(a, e, f)
  andGate(b, d, g)
  orGate(f, g, c)
}

val result = new Wire
f(new Wire(true), new Wire(true), result)
println(result.value)
f(new Wire(true), new Wire(false), result)
println(result.value)
f(new Wire(false), new Wire(false), result)
println(result.value)
f(new Wire(false), new Wire(true), result)
println(result.value)
