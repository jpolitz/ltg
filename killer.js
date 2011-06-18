var ix = 0;
function runPlan(machine, plan) {
    var currMove = plan.next();
    while(!isNullMove(currMove)) {
        machine.move(currMove.slot, currMove.card, currMove.type === "L");
        ix++
        currMove = plan.next();
    }
    print("Plans have taken " + ix + " turns.");
    return;
}

function kill(machine) {
    var attackerSlot = 7;
    var attackCardSlot = 3;
    var helpCardSlot = 4;
    var targetVitality = Math.ceil(machine.initVitality * 10/9);

    var count = 0;
    for(var i = 0; i < machine.maxSlots; i++) {
        while(machine.getProponentSlot(attackerSlot).vitality < 
              targetVitality * 2) {
            var plan = mkHelp(machine,
                              helpCardSlot,
                              attackerSlot,
                              attackerSlot,
                              machine.getProponentSlot(attackerSlot).vitality - 1);
            runPlan(machine, plan);
            count++
        }
        var plan = mkAttack(machine, 
                            attackCardSlot, 
                            attackerSlot, 
                            i, 
                            targetVitality);
        runPlan(machine, plan);
    }
    print("Took " + count + " helps.");
    machine.print();
}

try {
    kill(new Machine(255, 65000, 10000));
}
catch(e) {
    print("Error: " + e);
    print(e.extra);
}