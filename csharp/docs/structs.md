<!-- -*- markdown -*- -->

# Structs

## TODO:

 * document `GetStructInstanceValue`, `SetStructInstanceValue` in
   Shovel.Api (maybe write some examples);
 * add a similar function to list the keys in a struct;
 * add implementation for required primitives `isStruct` and
   `isStructInstance` (documented below);
 * optimize structs.   

It turns out that using hashes everywhere is slow. By doubling the
keys with every instance, it makes state larger and slows down
serialization.

A solution is to use structs. These work like hashes with grefdot and
setindexed (syntax-wise) but most of the time they map to simple array
accesses underneath.

A struct is created with `defstruct(<fields>)`, where fields is an array
of fields. It is an error to have duplicate fields in a structure.

Example:

    var point = defstruct(array('x', 'y'))
    
Structs can be instantiated with `make`:

    var p1 = make(point)
    // p1.x is null;
    // p2.x is null.
    
`make` can take arguments which are mapped to the fields, using the
order of fields from the call to `defstruct`:

    var p1 = make(point, 1, 2)
    // p1.x is 1;
    // p1.y is 2.
    
Dot access can be used to get and set structure fields.

    p2.x = p1.x + width
    p2.y = p2.x + height
    
A hash can be copied into a struct:

    var p1 = hashToStruct(point, hash('x', 10, 'y', 20, 'z', 30))
    // p1.x is 10;
    // p1.y is 20;
    // 'z' from the hash is ignored.
    
    var p2 = hashToStruct(point, hash('x', 10))
    // p1.x is 10;
    // p2.y is null.

A struct can be copied into a hash:

    var h = structToHash(make(point, 1, 2))
    // h is hash('x', 1, 'y', 2)

It's possible to test if an object is a struct:

    isStruct(point) // true
    
To test if an object is a struct instance belonging to a certain
instance:

    isStructInstance(make(point), point) // true

