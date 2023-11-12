let r = loop {
    break {
        break {
            break {
                break {
                    break {
                        break 5;
                    }
                }
            }
        }
    }
};

assert(r == 5, "break didn't work");

print("ok");