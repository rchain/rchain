module NAME-EQUIVALENCE
imports FREE

// A function that determines if two names are equivalent
// This will be used to determine equivalence on a send/receive
// Names are equivalent, modulo:
//        1. order of any processes in parallel (unless they form part of a pattern within a
//           pattern)
//        2. any `| Nil` , except the same caveat as (1)
//        3. arithmetic expressions on the top level
//        4. alpha equivalence
//        5. added evals/quotes, e.g. the names @{@Nil!(0)} and @{*@{@Nil!(0)}} are equivalent.

endmodule
