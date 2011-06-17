function error(extra) {
    throw {extra : extra, tag: "interp"};
}

var Ident = { invoke:
              function(machine) { 
                  return function(x) { return x; }
              }};

var Zero = { invoke:
             function(machine) {
                 return 0;
             }};

var Succ = { invoke:
             function(machine) {
                 return function(n) {
                     if(typeof n !== "number") {
                         return error("Succ");
                     }
                     return n + 1;
                 }
             }};

var Dbl = { invoke:
            function(machine) {
                return function(n) {
                    if(typeof n !== "number") {
                        return error("Dbl");
                    }
                    var dbl = n * 2;
                    if(dbl > machine.maxVitality) {
                        return machine.maxVitality;
                    }
                    return dbl;
                }
            }};

var Get = { invoke:
            function(machine) {
                return function(i) {
                    if(machine.validSlot(i)) {
                        return machine.proponent
                    }
            }
