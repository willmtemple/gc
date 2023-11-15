let iterations = 10000;

#[keyword]
fn bench(op, next) {
    let op_name = str(op);

    quote! {
        local it = 0;

        print("Benchmarking operator:", $op_name);

        let start = now();

        loop {
            if it.get() == iterations {
                break;
            }

            it.get() $op $next;

            it.set(it.get() + 1);
        }

        let duration = now(start);

        print("  Duration:", duration, "ns");
        print("      Each:", duration / iterations, "ns")
    }
}

bench + 7;
bench - 13;
bench * 9;
bench / 11;
bench % 17;
bench ** 3;