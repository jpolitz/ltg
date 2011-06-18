function id(x) { return x; }

var DEBUG = false;
function error(extra) {
    if(DEBUG) {
        print("ERROR: " + extra);
    }
    throw {extra : extra, tag: "interp"};
}

var Ident = { make:
              function(machine) { 
                  var f = function(x) { return x; };
                  f.card = "I";
                  return f;
              }};

var Zero = { make:
             function(machine) {
                 return 0;
             }};

var Succ = { make:
             function(machine) {
                 var f = function(n) {
                     if(typeof n !== "number") {
                         return error("Succ: " + n);
                     }
                     if(n < machine.maxVitality) {
                         return n + 1;
                     }
                     return machine.maxVitality;
                 };
                 f.card = "succ";
                 return f;
             }};

var Dbl = { make:
            function(machine) {
                var f = function(n) {
                    if(typeof n !== "number") {
                        return error("Dbl: n was " + n);
                    }
                    var dbl = n * 2;
                    if(dbl > machine.maxVitality) {
                        return machine.maxVitality;
                    }
                    return dbl;
                };
                f.card = "dbl";
                return f;
            }};

var Get = { make:
            function(machine) {
                var f = function(i) {
                    if(!machine.validSlot(i)) {
                        return error("Get");
                    }
                    return machine.getProponentSlot(i).field;
                };
                f.card = "get";
                return f;
            }};

var Put = { make:
            function(machine) {
                var f = function(i) { return id; };
                f.card = "put";
                return f;
            }};

// Card "S" is a function that takes an argument f and returns another
// function, which (when applied) will take another argument g and
// return yet another function, which (when applied) will take yet
// another argument x, apply f to x obtaining a return value h (or
// raise an error if f is not a function), apply g to x obtaining
// another return value y (or raise an error if g is not a function),
// apply h to y obtaining yet another return value z (or raise an
// error if h is not a function), and return z. [Remark: The first
// function is called the S combinator and written λf.λg.λx.fx(gx) in
// lambda-calculus.]
var S = { make:
          function(machine) {
              var f = function(f) {
                  var g = function(g) {
                      var x = function(x) {
                          if(typeof f !== "function") {
                              return error("S - f");
                          }
                          var h = f(x);
                          if(typeof g !== "function") {
                              return error("S - g");
                          }
                          var y = g(x);
                          if(typeof h !== "function") {
                              return error("S - h");
                          }
                          var z = h(y);
                          return z;
                      };
                      x.card = "Sx";
                      return x;
                  };
                  g.card = "Sg";
                  return g;
              };
              f.card = "Sf";
              return f;
          }};

// Card "K" is a function that takes an argument x and returns another
// function, which (when applied) will take another (unused) argument
// y and return x. [Remark: The first function is called the K
// combinator and written λx.λy.x in lambda-calculus.]
var K = { make:
          function(machine) {
              var f = function(x) {
                  var g = function(y) {
                      return x;
                  };
                  g.card = "Ky";
                  return g;
              };
              f.card = "Kx";
              return f;
          }};

// Card "inc" is a function that takes an argument i, and
//
// increases by 1 the vitality v of the ith slot of the proponent if
// v>0 and v<65535,
//
// does nothing if v=65535 or v<=0, or
//
// raises an error if i is not a valid slot number,
//
// and returns the identity function.
var Inc = { make:
            function(machine) {
                var f = function(i) {
                    if(!machine.validSlot(i)) {
                        return error("Inc");
                    }
                    var slot = machine.getProponentSlot(i);
                    if(!machine.zombie) {
                        if(slot.vitality < machine.maxVitality && 
                           slot.vitality > 0) {
                            slot.vitality += 1;
                        }
                    }
                    else {
                        if(slot.vitality > 0) {
                            slot.vitality -= 1;
                        }
                    }
                    return id;
                };
                f.card = "Inc";
                return f;
            }};

var Dec = { make:
            function(machine) {
                var f = function(i) {
                    if(!machine.validSlot(i)) {
                        return error("Dec");
                    }
                    var slot = machine.getOpponentSlot(i);
                    if(!machine.zombie) {
                        if(slot.vitality > 0) {
                            slot.vitality -= 1;
                        }
                    }
                    else {
                        if(slot.vitality < machine.maxVitality &&
                           slot.vitality > 0) {
                            slot.vitality += 1;
                        }
                    }
                    return id;
                };
                f.card = "Dec";
                return f;
            }};

// Card "attack" is a function that takes an argument i and returns
// another function, which (when applied) will take another argument j
// and return yet another function, which (when applied) will take yet
// another argument n, decrease by n the vitality v of the ith slot of
// the proponent (or raise an error if i is not a valid slot number, n
// is not an integer, or n is greater than v), and

// decrease by n*9/10 (n times 9 divided by 10, with the remainder
// discarded) the vitality w of the (255-j)th slot of the opponent if
// it is alive (w is set to 0 if it would become less than 0 by this
// decrease),

// do nothing if the slot is dead, or

// raise an error if j is not a valid slot number,

// and return the identity function.
var Attack = { make:
               function(machine) {
                   var f = function(i) {
                       var g = function(j) {
                           var h = function(n) {
                               if(!machine.validSlot(i)) {
                                   return error("Attack - i");
                               }
                               if(typeof n !== "number") {
                                   return error("Attack - n");
                               }
                               print("Attack: " + i + ", " + j + ", " + n);
                               var proSlot = machine.getProponentSlot(i);
                               if(n > proSlot.vitality) {
                                   return error("Attack - n>v");
                               }
                               proSlot.vitality -= n;

                               if(!machine.validSlot(j)) {
                                   return error("Attack - j");
                               }
                               var oppSlot = machine.getOpponentSlotOpposite(j);
                               if(machine.deadSlot(oppSlot)) {
                                   return id;
                               }
                               var newVitality;
                               if(!machine.zombie) {
                                   newVitality = oppSlot.vitality - Math.floor(n * (9/10));
                                   if(newVitality < 0) { newVitality = 0 }
                               }
                               else {
                                   newVitality = oppSlot.vitality + Math.floor(n * (9/10));
                                   if(newVitality > machine.maxVitality) {
                                       newVitality = machine.maxVitality;
                                   }
                               }
                               oppSlot.vitality = newVitality;
                               return id;
                           };
                           h.card = "AttackN";
                           return h;
                       };
                       g.card = "AttackJ";
                       return g;
                   };
                   f.card = "AttackI";
                   return f;
               }};

// Card "help" is a function that takes an argument i and returns
// another function, which (when applied) will take another argument j
// and return yet another function, which (when applied) will take yet
// another argument n, decrease by n the vitality v of the ith slot of
// the proponent (or raise an error if i is not a valid slot number, n
// is not an integer, or n is greater than v), and

// increase by n*11/10 (n times 11 divided by 10, with the remainder
// discarded) the vitality w of the jth slot of the proponent if it is
// alive (w is set to 65535 if it would become greater than 65535 by
// this increase),

// do nothing if the slot is dead, or

// raise an error if j is not a valid slot number,

// and return the identity function.
var Help = { make:
             function(machine) {
                 var f = function(i) {
                     var g = function(j) {
                         var h = function(n) {
                             if(!machine.validSlot(i)) {
                                 return error("Help - i");
                             }
                             var paySlot = machine.getProponentSlot(i);
                             if(typeof n !== "number") {
                                 return error("Help - n not number");
                             }
                             if(n > paySlot.vitality) {
                                 return error("Help - n > v");
                             }
                             print("Help " + i + ", " + j + ", " + n);
                             print("Paying at " + paySlot.vitality);

                             paySlot.vitality -= n;

                             if(!machine.validSlot(j)) {
                                 return error("Help - j");
                             }
                             var gainSlot = machine.getProponentSlot(j);
                             var newVitality;
                             if(!machine.zombie) {
                                 print("Gaining at " + gainSlot.vitality);
                                 newVitality = gainSlot.vitality + Math.floor((11/10) * n);
                                 if(newVitality > machine.maxVitality) {
                                     newVitality = machine.maxVitality;
                                 }
                             }
                             else {
                                 newVitality = gainSlot - Math.floor((11/10) * n);
                                 if(newVitality < 0) {
                                     newVitality = 0;
                                 }

                             }
                             gainSlot.vitality = newVitality;
                             machine.print();
                             return id;
                         };
                         h.card = "HelpN";
                         return h;
                     };
                     g.card = "HelpJ";
                     return g;
                 };
                 f.card = "HelpI";
                 return f;
             }};

// Card "copy" is a function that takes an argument i, and returns the
// value of the field of the ith slot of the opponent. It raises an
// error if i is not a valid slot number. Note that the slot is ith,
// not (255-i)th.
var Copy = { make:
             function(machine) {
                 var f = function(i) {
                     if(!machine.validSlot(i)) {
                         return error("Copy - i");
                     }
                     return machine.getOpponentSlot(i).field;
                 };
                 f.card = "Copy";
                 return f;
             }};

var Revive = { make:
               function(machine) {
                   var f = function(i) {
                       if(!machine.validSlot(i)) {
                           return error("Revive - i");
                       }
                       var saveSlot = machine.getProponentSlot(i);
                       if(saveSlot.vitality <= 0) {
                           saveSlot.vitality = 1;
                       }
                       return id;
                   };
                   f.card = "Revive";
                   return f;
               }};

var Zombie = { make:
               function(machine) {
                   var f = function(i) {
                       var g = function(x) {
                           if(!machine.validSlot(i)) {
                               return error("Zombie - i");
                           }
                           var slot = machine.getOpponentSlotOpposite(i);
                           if(!machine.deadSlot(slot)) {
                               return error("Zombie - slot not dead");
                           }
                           slot.field = x;
                           slot.vitality = -1;
                           return id;
                       };
                       g.card = "ZombieX";
                       return g;
                   };
                   f.card = "ZombieI";
                   return f;
               }};

var CARDS =
    {"I" : Ident,
     "zero" : Zero,
     "succ" : Succ,
     "dbl" : Dbl,
     "get" : Get,
     "put" : Put,
     "S" : S,
     "K" : K,
     "inc" : Inc,
     "dec" : Dec,
     "attack" : Attack,
     "help" : Help,
     "copy" : Copy,
     "revive" : Revive,
     "zombie" : Zombie}

var mkCards = function(machine) {
    return {
        "I" : Ident(machine),
        "zero" : Zero(machine),
        "succ" : Succ(machine),
        "dbl" : Dbl(machine),
        "get" : Get(machine),
        "put" : Put(machine),
        "S" : S(machine),
        "K" : K(machine),
        "inc" : Inc(machine),
        "dec" : Dec(machine),
        "attack" : Attack(machine),
        "help" : Help(machine),
        "copy" : Copy(machine),
        "revive" : Revive(machine),
        "zombie" : Zombie(machine)}
}
