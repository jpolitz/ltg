function selectPlan(machine) {
    return mkChain(mkNum(machine, 0, 54),
                   mkCopy(machine, 0, 7));
}

function play(machine) {
    var currMove = null;
    var currPlan = selectPlan(machine);
    currMove = currPlan.next();
    do {
        machine.move(currMove.slot, currMove.card, currMove.type === "L");
        currMove = currPlan.next();
        machine.print();
    } while(!isNullMove(currMove));
}

play(new Machine(10, 100, 50));