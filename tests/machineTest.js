function testMachine() {
    print("new Machine(10, 255, 1000);");
    var m = new Machine(10, 255, 1000);

    print("Should have 10 as a valid slot");
    if(m.validSlot(10)) { print("Yes"); }
    else { print("Failed"); }

    print("Should have 0 as a valid slot");
    if(m.validSlot(0)) { print("Yes"); }
    else { print("Failed"); }

    print("Should have 11 as an invalid slot");
    if(m.validSlot(11)) { print("Failed"); }
    else { print("Yes"); }

    print("Should have -1 as an invalid slot");
    if(m.validSlot(-1)) { print("Failed"); }
    else { print("Yes"); }
}

testMachine();