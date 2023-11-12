for k in [1, 2, 3] {
    print("Iterating", k);
};

#[keyword]
fn dang(...body) {
    quote! {
        let r = $(...body);

        print("dang", r);

        r
    }
}

dang "son";