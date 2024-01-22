rec {
  pair = name: value:
    { inherit name value; };

  swap = op: a: b: op b a;

  max = a: b: if b > a then b else a;
  min = a: b: if b < a then b else a;

  attrset = rec {
    empty = { };
    singleton = name: value: { "${name}" = value; };

    get = name: attr: attr.${name};
    insert = name: value: attr: union (singleton name value) attr;
    remove = name: attr: builtins.removeAttrs attr [ name ];
    update = name: op: attr:
      let
        original = attr.${name} or null;
        new = op original;
      in
      if new == null then
        remove name attr
      else
        insert name new attr;

    union = new: prev: prev // new;
    updateFull = op: prev: prev // (op prev);

    # ops => list of (final: prev: { })
    chain = ops: prev:
      let
        final = builtins.foldl'
          (prev: op: prev // (op final prev))
          prev
          ops;
      in
      final;

    removeMany = names: attr: builtins.removeAttrs attr names;

    # ops => (name: value: accu: newValue)
    foldl = op: init: prev:
      builtins.foldl'
        (accu: name: op name prev.${name} accu)
        init
        (builtins.attrNames prev);
    foldl' = op: swap (foldl op);

    foldr = op: init: prev:
      list.foldr
        (name: accu: op name prev.${name} accu)
        init
        (builtins.attrNames prev);
    foldr' = op: swap (foldr op);

    isEmpty = attr: attr == { };
    member = name: attr: attr ? "${name}";
    size = attr: builtins.length (builtins.attrNames attr);

    names = attr: builtins.attrNames attr;
    values = attr: builtins.attrValues attr;

    toList = attr: builtins.map (name: pair name attr.${name}) (builtins.attrNames attr);
    fromList = xs: builtins.listToAttrs xs;

    # op => (name: value: newValue)
    map = op: prev:
      builtins.foldl'
        (accu: name: accu // (singleton name (op name prev.${name})))
        { }
        (builtins.attrNames prev);

    # op => (name: value: Bool)
    filter = op: prev:
      builtins.listToAttrs
        (builtins.filter
          ({ name, value }: op name value)
          (toList prev)
        );

    # op => (name: value: Bool)
    partition = op: prev:
      attrset.map
        (_: val: builtins.listToAttrs val)
        (builtins.partition
          ({ name, value }: op name value)
          (toList prev)
        );

    intersect = attrA: attrB:
      builtins.listToAttrs
        (builtins.filter
          ({ name, value }: attrB ? "${name}")
          (toList attrA)
        );

    diff = attrA: attrB:
      builtins.listToAttrs
        (builtins.filter
          ({ name, value }: ! attrB ? "${name}")
          (toList attrA)
        );

    # whenOnlyA => (name: value: accu: newValue)
    # whenBoth => (name: valueA: valueB: accu: newValue)
    # whenOnlyB => (name: value: accu: newValue)
    merge = whenOnlyA: whenBoth: whenOnlyB: attrA: attrB: init:
      builtins.foldl'
        (accu: name:
          let
            isOnA = attrA ? "${name}";
            isOnB = attrB ? "${name}";
            isBoth = isOnA && isOnB;
          in
          if isBoth then
            whenBoth name attrA.${name} attrB.${name} accu
          else if isOnA then
            whenOnlyA name attrA.${name} accu
          else
            whenOnlyB name attrB.${name} accu
        )
        init
        (builtins.attrNames (attrB // attrA));
  };

  list = rec {
    empty = [ ];
    singleton = x: [ x ];

    inherit (builtins) all any concatMap filter head length map partition tail;

    # Building lists with this is inefficient, use builtins.genList instead.
    cons = x: xs: [ x ] ++ xs;
    push = x: xs: xs ++ [ x ];

    # op => (value: accu: newValue)
    foldl = op: init: prev:
      builtins.foldl'
        (accu: value: op value accu)
        init
        prev;
    foldl' = op: swap (foldl op);

    # taken from nixpkgs
    foldr = op: init: prev:
      let
        len = builtins.length prev;
        foldOp = n:
          if n == len
          then init
          else op (builtins.elemAt prev n) (foldOp (n + 1));
      in
      foldOp 0;
    foldr' = op: swap (foldr op);

    repeat = n: x:
      if n <= 0 then [ ]
      else if n == 1 then [ x ]
      else builtins.genList (_: x) n;

    range = min: max:
      if min > max then [ ]
      else if min == max then [ max ]
      else builtins.genList (n: min + n) (max - min + 1);

    # op => (n: value: newValue)
    indexedMap = op: xs:
      let len = builtins.length xs;
      in
      builtins.genList (n: op n (builtins.elemAt xs n)) len;

    filterMap = op: xs:
      builtins.foldl'
        (accu: value:
          let
            new = op value;
          in
          if new == null then
            accu
          else
            accu ++ [ new ]
        )
        [ ]
        xs;

    # taken from nixpkgs
    reverse = xs:
      let len = length xs;
      in builtins.genList
        (n: builtins.elemAt xs (len - n - 1))
        len;

    member = x: xs:
      let
        len = builtins.length xs;
        quit = len - 1;
        search = n:
          if builtins.elemAt xs n == x then
            true
          else if n == quit then
            false
          else search (n + 1);
      in
      if len == 0 then false
      else search 0;

    maximum = xs:
      if xs == [ ] then null
      else
        builtins.foldl'
          (last: x:
            if x > last then x
            else last
          )
          (builtins.head xs)
          xs;

    minimun = xs:
      if xs == [ ] then null
      else
        builtins.foldl'
          (last: x:
            if x < last then x
            else last
          )
          (builtins.head xs)
          xs;

    sum = xs:
      builtins.foldl'
        builtins.add
        0
        xs;

    product = xs:
      builtins.foldl'
        builtins.mul
        1
        xs;

    append = new: prev: prev ++ new;
    prepend = new: prev: new ++ prev;

    concat = xss: builtins.concatLists xss;

    intersperse = sep: xs: builtins.concatMap (x: [ x sep ]) xs;

    map2 = op: xs: ys:
      builtins.genList
        (n: op (builtins.elemAt xs n) (builtins.elemAt ys n))
        (min (builtins.length xs) (builtins.length ys));

    map3 = op: xs: ys: zs:
      builtins.genList
        (n: op (builtins.elemAt xs n) (builtins.elemAt ys n) (builtins.elemAt zs n))
        (minimun [ (builtins.length xs) (builtins.length ys) (builtins.length zs) ]);

    map4 = op: xs: ys: zs: ws:
      builtins.genList
        (n: op (builtins.elemAt xs n) (builtins.elemAt ys n) (builtins.elemAt zs n) (builtins.elemAt ws n))
        (minimun [ (builtins.length xs) (builtins.length ys) (builtins.length zs) (builtins.length ws) ]);

    map5 = op: xs: ys: zs: ws: ks:
      builtins.genList
        (n: op (builtins.elemAt xs n) (builtins.elemAt ys n) (builtins.elemAt zs n) (builtins.elemAt ws n) (builtins.elemAt ks n))
        (minimun [ (builtins.length xs) (builtins.length ys) (builtins.length zs) (builtins.length ws) (builtins.length ks) ]);

    sort = xs: builtins.sort builtins.lessThan xs;
    sortBy = name: xs: builtins.sort (a: b: (attrset.get name a) < (attrset.get name b)) xs;
    sortWith = op: xs: builtins.sort op xs;

    isEmpty = xs: xs == [ ];

    take = n: xs: builtins.genList (builtins.elemAt xs) n;
    drop = n: xs: builtins.genList (i: builtins.elemAt xs (n + i)) (builtins.length xs - n);
  };
}
