local x = 0;

loop {
    let next = x.get() + 1;
    x.set(next);

    print("x =", next);
}

print("All done.");