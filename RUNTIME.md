* Garbage collector
    * Long-term goal: write the GC itself in GRIN. This requires some reified representation of the GRIN datastructures, i.e. an ADT capturing the Node types, which we can coerce into a runtime state. It would essentially be the dual of what a debugger does: take the runtime state and turn it into a traversable datastructure.
    * For now we should write it in Rust or similar low-level language, KISS.
* Threading
* FFI
* Debugging/inspection
* Dynamic bytecode loading
    * Interactive prompt to runtime?
* JIT compiler
* Tracer
* Stats/profiling - memory consumption etc

* Read
  * lhc notes: https://github.com/Lemmih/lhc/blob/master/bedrock/NOTES
  * Disruptor: https://lmax-exchange.github.io/disruptor/files/Disruptor-1.0.pdf
  * JVM GC: http://info.azul.com/rs/090-OKK-659/images/WP-Understanding%20Java%20Garbage%20Collection%20-%2020170110.pdf
  * Hotspot parallel gc: http://www.oracle.com/technetwork/java/javase/tech/memorymanagement-whitepaper-1-150020.pdf
  * http://judy.sourceforge.net/doc/10minutes.htm