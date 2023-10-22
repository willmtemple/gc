use proc_macro2::{Span, TokenStream};
use quote::quote;
use syn::{
    parse_macro_input, Data, DataEnum, DataStruct, DeriveInput, Field, Fields, FieldsNamed,
    FieldsUnnamed, Ident, Variant,
};

#[proc_macro_derive(Mark)]
pub fn derive_mark(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput {
        ident,
        generics,
        data,
        ..
    } = parse_macro_input!(input as DeriveInput);

    let body = derive_body(ident.clone(), data);

    let expanded = quote! {
        impl #generics Mark for #ident #generics {
            fn mark(&mut self) {
                #body
            }
        }
    };

    proc_macro::TokenStream::from(expanded)
}

fn derive_body(t_ident: Ident, data: Data) -> TokenStream {
    match data {
        Data::Struct(DataStruct {
            fields: Fields::Unit,
            ..
        }) => TokenStream::new(),
        Data::Struct(DataStruct {
            fields: Fields::Unnamed(FieldsUnnamed { unnamed, .. }),
            ..
        }) => {
            let mut body = TokenStream::new();
            for (i, Field { ty, .. }) in unnamed.into_iter().enumerate() {
                body.extend(quote! {
                    <#ty as Mark>::mark(&mut self.#i);
                });
            }
            body
        }
        Data::Struct(DataStruct {
            fields: Fields::Named(FieldsNamed { named, .. }),
            ..
        }) => {
            let mut body = TokenStream::new();
            for Field { ident, ty, .. } in named.into_iter() {
                let ident = ident.unwrap();
                body.extend(quote! {
                    <#ty as Mark>::mark(&mut self.#ident);
                });
            }
            body
        }
        Data::Enum(DataEnum { variants, .. }) => {
            let mut body = TokenStream::new();

            for Variant {
                discriminant,
                fields,
                ident,
                ..
            } in variants.into_iter()
            {
                body.extend({
                    if discriminant.is_some() {
                        quote! {
                            #t_ident::#ident => {}
                        }
                    } else {
                        let mut body = TokenStream::new();
                        match &fields {
                            Fields::Unit => {}
                            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                                for (i, Field { ty, .. }) in unnamed.into_iter().enumerate() {
                                    let ident = Ident::new(&format!("v{}", i), Span::call_site());
                                    body.extend(quote! {
                                        <#ty as Mark>::mark(#ident);
                                    });
                                }
                            }
                            Fields::Named(FieldsNamed { named, .. }) => {
                                for Field { ident, ty, .. } in named.into_iter() {
                                    let ident = ident.clone().unwrap();
                                    body.extend(quote! {
                                        <#ty as Mark>::mark(#ident);
                                    });
                                }
                            }
                        }

                        let binder = match fields {
                            Fields::Unit => TokenStream::new(),
                            Fields::Unnamed(FieldsUnnamed { unnamed, .. }) => {
                                let names = (0..unnamed.len())
                                    .map(|i| Ident::new(&format!("v{}", i), Span::call_site()));
                                quote!((#(#names),*))
                            }
                            Fields::Named(FieldsNamed { named, .. }) => {
                                let names = named.iter().map(|f| f.ident.as_ref().unwrap());
                                quote!({ #(#names),* })
                            }
                        };

                        quote! {
                            #t_ident::#ident #binder => {
                                #body
                            }
                        }
                    }
                });
            }

            quote! {
                match self {
                    #body
                }
            }
        }
        Data::Union(_) => unimplemented!("Unions cannot implement Mark"),
    }
}
