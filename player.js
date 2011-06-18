function selectPlan(machine) {
    print("planselect");
    return mkChain(mkChain(mkNum(machine, 0, 54),
                           mkCopy(machine, 0, 7)),
                   mkApply(machine, 5, 7));
}

function play(machine) {
    var currMove = null;
    var currPlan = selectPlan(machine);
    currMove = currPlan.next();
    do {
        print(currMove.slot);
        machine.move(currMove.slot, currMove.card, currMove.type === "L");
        currMove = currPlan.next();
        machine.print();
    } while(!isNullMove(currMove));
}

play(new Machine(10, 100, 50));
