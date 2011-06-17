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

function testSucc(m) {
    print("Succ should increment");
    expectEqual(Succ.make(m)(4), 5);
    
    print("Succ should error on non-numbers");
    try {
        var excepted = false;
        Succ.make(m)(function() {});
    } catch(e) { excepted = true; 
                 expectEqual(e.tag, "interp"); }
    expect(excepted);

    print("Succ should not increment maxvitality");
    expectEqual(Succ.make(m)(m.maxVitality), m.maxVitality);
}

function testDbl(m) {
    print("Dbl should double");
    expectEqual(Dbl.make(m)(4), 8);

    print("Dbl should not double past max vit");
    expectEqual(Dbl.make(m)(m.maxVitality / 1.5), m.maxVitality);

    print("Dbl should error on non-numbers");
    try {
        var excepted = false;
        Dbl.make(m)(function() {});
    } catch(e) { excepted = true; 
                 expectEqual(e.tag, "interp"); }
    expect(excepted);
}

function testGet(m) {
    print("Get should return the proponent's field");
    m.player0Slots[0].field = 52;
    expectEqual(Get.make(m)(0), 52);

    print("Get should return an error if an invalid slot is given");
    try {
        var excepted = false;
        Dbl.make(m)(m.maxSlots + 5);
    } catch(e) { excepted = true; 
                 expectEqual(e.tag, "interp"); }
    expect(excepted);
}

var m = new Machine(10, 255, 100);
testIdent(m);
testZero(m);
testSucc(m);
testDbl(m);
testGet(m);
//testPut(m);
//testS();
//testK();
//...