var errorPlan = function(extra) {
    throw {tag: "Plan", extra: extra};
}

// Plans return moves or nullPlan
// moves are {type: "L"|"R", slot: slotNum, card: card}
var nullMove = {};

var isNullMove = function(move) { return move === nullMove; }

// Return a plan to put number n in slot i
// Takes 2lg(n) steps in the worst case
// Expects i to contain the identity function, errorPlans if it does not
function mkNum(machine, i, n) {
    if(!machine.validSlot(i)) {
        return errorPlan("Bad slot for mkNum");
    }
    if(machine.getProponentSlot(i).field !== id) {
        return errorPlan("Not an id function in slot " + i + " to mkNum!!!");
    }
    if(typeof n !== "number") { return errorPlan("n not a number in mkNum"); }

    function getList(n) {
        if(n == 0) { return [{type: "R", card: Zero, slot: i}]; }
        if((n % 2) == 0) { 
            var arr = getList(n / 2);
            print(arr);
            arr[arr.length] = {type: "L", card: Dbl, slot: i};
            return arr;
        }
        var arr = getList(n - 1);
        print(arr);
        arr[arr.length] = {type: "L", card: Succ, slot: i};
        return arr;
    }

    var list = getList(n);
    var ix = 0;
    return {
        next: function() {
            if(ix >= list.length) { return nullMove; }
            var move = list[ix];
            ix += 1;
            return move;
        }};
}

function mkCopy(machine, i, j) {
    var iPlan = mkNum(machine, j, i);
    var done = false;

    return {
        next: function() {
            var move = iPlan.next();
            if(isNullMove(move)) {
                if(done) { return nullMove; }
                else { 
                    done = true;
                    return {type: "L", card: Get, slot: j}; 
                }
            } else {
                return move;
            }
        }};
}

// Assume that slot I is a function, and
// apply that to the number n
function mkApplyN(machine, i, n) {
    if (typeof n !== 'number') {
        return errorPlan("Not a number in mkApplyN: " + n);
    }
    if (typeof i !== 'number') {
        return errorPlan("Not a number in mkApplyN: " + i);
    }

    function getList(n) {
        if(n == 0) { 
            return [{type: "R", card: Zero, slot: i}] 
        }
        if((n % 2) == 0) { 
            var arr = getList(n / 2);
            var cmd = [{type: "L", card: K, slot: i},
                       {type: "L", card: S, slot: i},
                       {type: "R", card: Dbl, slot: i}];

            return cmd.concat(arr);
        }
        var arr = getList(n - 1);
        var cmd = [{type: "L", card: K, slot: i},
                   {type: "L", card: S, slot: i},
                   {type: "R", card: Succ, slot: i}];
        return cmd.concat(arr);
    }
    var list = getList(n);
    var ix = 0;
    return {
        next: function() {
            if(ix >= list.length) {
                return nullMove;
            }
            var move = list[ix];
            ix += 1;
            return move;
        }};
}

// applies i to j, with the result in i
function mkApply(machine, i, j) {
    var list = [{type: "L", card: K, slot: i},
                {type: "L", card: S, slot: i},
                {type: "R", card: Get, slot: i}];
    var cPlan = mkApplyN(machine, i, j);
    var ix = 0;

    return {
        next: function() {
            if(ix >= list.length) {
                return cPlan.next();
            }
            var move = list[ix];
            ix += 1;
            return move;
        }};
}

function mkPut(machine, i, card) {
    var done = 0;
    return {
        next: function() {
            if(done) return nullMove;
            done = true; 
            return {type: "R", card: card, slot: i};
        }};
}

function mkAttack(machine, atkSlot, i, j, n) {
    return mkChainList([mkPut(machine, atkSlot, Attack),
                        mkApplyN(machine, atkSlot, i),
                        mkApplyN(machine, atkSlot, j),
                        mkApplyN(machine, atkSlot, n)]);
}

function mkHelp(machine, helpSlot, i, j, n) {
    return mkChainList([mkPut(machine, helpSlot, Help),
                        mkApplyN(machine, helpSlot, i),
                        mkApplyN(machine, helpSlot, j),
                        mkApplyN(machine, helpSlot, n)]);
}

function mkChain(plan1, plan2) {
    return {
        next: function() {
            var move = plan1.next();
            if(isNullMove(move)) {
                move = plan2.next();
            }
            return move;
        }};
}

function mkChainList(plans) {
    if(plans.length === 1) { return plans[0]; }
    return mkChain(plans[0], mkChainList(plans.slice(1)));
}

