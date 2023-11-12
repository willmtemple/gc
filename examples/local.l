local x = 0;

loop {
    let next = x.get() + 1;
    x.set(next);

    if (next % 1000) == 0 {
        print("x =", next);
    }
}

print("All done.");