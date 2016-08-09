contract Functor with f {
    void fmap(void func(const a *in, b *out), const f a *fin, f b *fout);
}

impl Functor by (Array n) {
    void fmap(void func(const a *in, b *out), const Array n a *as, Array n b *bs) {
        for (uint32_t i = 0 ; i < length(as); i++) {
            func(&as[i], &bs[i]);
        }
    }
}

union Maybe t {
    Nothing,
    Just { t value; },
};

impl Functor by Maybe {
    void fmap(void func(const a *in, b *out), const Maybe a *fin, Maybe b *fout) {
        switch (*fin) {
        case Nothing: *fout = Nothing;
        case Just(x): {
            func(&x, &(fout as Just)->value);
        }
        }
    }
}

contract Read with t {
    void read(const char *, Maybe t *out);
}

contract Service with s {
    DrainResult drain(s *);
}

impl Service by OrcClient {
}
