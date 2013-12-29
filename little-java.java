interface TI {
    o2oI apply(TI x);
}

interface o2oI {
    Object apply(Object x);
}

interface oo2ooI {
    o2oI apply(o2oI x);
}

interface oo2oo2ooI {
    o2oI apply(oo2ooI x);
}

class Y implements oo2oo2ooI {
    public o2oI apply(oo2ooI f) {
	return new H(f).apply(new H(f));
    }
}

class H implements TI {
    oo2ooI f;
    H(oo2ooI _f) {
	f = _f;
    }
    public o2oI apply(TI x) {
	return f.apply(new G(x));
    }
}

class G implements o2oI {
    TI x;
    G(TI _x) {
	x = _x;
    }
    public Object apply(Object y) {
	return (x.apply(x)).apply(y);
    }
}

class MkFact implements oo2ooI {
    public o2oI apply(o2oI fact) {
	return Fact(fact);
    }
}

class Fact implements o2oI {
    o2oI fact;
    Fact(o2oI _fact) {
	fact = _fact;
    }
    public Object apply(Object i) {
	int inti == ((Integer)i).intValue();
	if (inti == 0) {
	    return new Integer(1);
	} else {
	    return new Integer(inti * ((Integer)fact.apply(new Integer(inti - 1))).intValue());
	}
    }
}
