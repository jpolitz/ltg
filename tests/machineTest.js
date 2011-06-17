function expectEqual(val, testVal) {
    if(val !== testVal) { 
        print("!!!Failed: " + String(val) + " !== " + String(testVal));
    }
}

function expect(val) {
    if(val) { expectEqual(true, true); }
}

function testMachine() {
    var m = new Machine(10, 255, 1000);

    print("Should have 10 as a valid slot");
    expect(m.validSlot(10));

    print("Should have 0 as a valid slot");
    expect(m.validSlot(0));

    print("Should have 11 as an invalid slot");
    expect(m.validSlot(11));

    print("Should have -1 as an invalid slot");
    expect(m.validSlot(-1));

    print("First slot should contain ident");
    var slot0 = m.getProponentSlot(0);
    expect(slot0.field(52) === 52);
    
}

testMachine();

function testIdent(m) {
    var o = {};
    print("Ident should make an ident");
    expectEqual(Ident.make(m)(o), o);
}

function testZero(m) {
    print("Zero should make to zero");
    expectEqual(Zero.make(m), 0);
}

testIdent();
testZero();
