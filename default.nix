rec {
  "+" = builtins.add;
  "-" = builtins.sub;
  "*" = builtins.mul;
  "/" = builtins.div;
  "^" = _: _: throw "Exponential is missing in Nix";

  "==" = a: b: a == b;
  "!=" = a: b: a != b;
  ">" = a: b: a > b;
  "<" = builtins.lessThan;
  ">=" = a: b: a >= b;
  "<=" = a: b: a <= b;

  "!" = x: !x;
  "&&" = a: b: a && b;
  "||" = a: b: a || b;
  xor = a: b: a != b && (a || b);

  max = a: b: if b > a then b else a;
  min = a: b: if b < a then b else a;
  clamp = left: right: x:
    if x < left then left
    else if x > right then right
    else x;

  toFloat = x: x * 1.0;

  pair = name: value:
    { inherit name value; };

  swap = op: a: b: op b a;

  identity = x: x;
  always = x: _: x;

  "<|" = fn: a: fn a;
  "|>" = a: fn: fn a;
  "<<" = fnB: fnA: a:
    fnB (fnA a);
  ">>" = fnA: fnB: a:
    fnB (fnA a);

  math = rec {
    inherit (builtins) floor;
    ceiling = builtins.ceil;
    truncate = x:
      if x >= 0 then builtins.floor x
      else builtins.ceil x;
    round = _: throw "Round is missing in Nix";

    floatDiv = a: b:
      if builtins.isInt a then (toFloat a) / b
      else builtins.div a b;
    integerDiv = a: b:
      if builtins.isFloat a then (truncate a) / b
      else builtins.div a b;

    # https://web.archive.org/web/20240125182710/https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
    modBy = modulus: x:
      let answer = x - (integerDiv x modulus) * modulus;
      in
      if ((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
      then answer + modulus
      else answer;
    remainderBy = modulus: x: x - (integerDiv x modulus) * modulus;

    negate = x: x * (-1);
    abs = x: if x < 0 then x * (-1) else x;
  };

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

    getFromMany = name: xs: builtins.catAttrs name xs;

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

    intersect = attrA: attrB: builtins.intersectAttrs attrB attrA;

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

    # op => (name: values: newMergedValue)
    zipWith = op: xs: builtins.zipAttrsWith op xs;
  };

  bitwise = {
    and = builtins.bitAnd;
    or = builtins.bitOr;
    xor = builtins.bitXor;
    complement = builtins.bitNot;
  };

  list = rec {
    empty = [ ];
    singleton = x: [ x ];

    fromIndex = op: len: builtins.genList op len;
    fromClosure = args: len: builtins.genList args len;

    inherit (builtins) all any concatMap filter head length map partition tail;

    # Building lists with this is inefficient, use builtins.genList instead.
    cons = x: xs: [ x ] ++ xs;
    push = x: xs: xs ++ [ x ];

    last = xs:
      let len = builtins.length xs;
      in
      if len > 1 then builtins.elemAt xs (len - 1)
      else builtins.head xs;

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
      let
        len = length xs;
        tailIx = len - 1;
      in
      builtins.genList
        (n: builtins.elemAt xs (tailIx - n))
        len;

    member = x: xs: builtins.elem x xs;

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
    takeLast = n: xs:
      let offset = builtins.length xs - n;
      in
      builtins.genList (i: builtins.elemAt xs (i + offset)) n;
    drop = n: xs: builtins.genList (i: builtins.elemAt xs (n + i)) (builtins.length xs - n);
    dropLast = n: xs: builtins.genList (builtins.elemAt xs) (builtins.length xs - n);

    # op => (item: patternFromItem)
    groupBy = op: xs: builtins.groupBy op xs;
  };

  # This deserves a UTF-8 refactor if something as https://github.com/figsoda/utf8 gets upstreamed
  string = rec {
    empty = "";
    isEmpty = str: str == "";

    length = str: builtins.stringLength str;

    toList = str:
      let len = builtins.stringLength str;
      in
      builtins.genList (n: builtins.substring n 1 str) len;
    fromList = xs: builtins.concatStringsSep "" xs;

    reverse = prev:
      let
        len = builtins.stringLength prev;
        tailIx = len - 1;
        newList = builtins.genList (n: builtins.substring (tailIx - n) 1 prev) len;
      in
      builtins.concatStringsSep "" newList;

    repeat = n: x:
      if n <= 0 then ""
      else if n == 1 then x
      else builtins.concatStringsSep "" (builtins.genList (_: x) n);

    replace = from: to: prev: builtins.replaceStrings [ from ] [ to ] prev;
    replaceMany = allFrom: allTo: prev: builtins.replaceStrings allFrom allTo prev;

    append = new: prev: prev + new;
    concat = xs: builtins.concatStringsSep "" xs;

    # taken from nixpkgs
    escape = list: builtins.replaceStrings list (builtins.map (c: "\\${c}") list);

    split = pattern: str:
      let
        safePattern = escape ssot.regexSpecialCharacters pattern;
        regexResult = builtins.split "(${safePattern})" str;
      in
      builtins.filter builtins.isString regexResult;

    join = sep: xs: builtins.concatStringsSep sep xs;

    words = str:
      let
        regexResult = builtins.split "([[:space:]]+)" str;
      in
      builtins.filter builtins.isString regexResult;

    lines = str:
      let
        regexResult = builtins.split "(\r?\n)+" str;
      in
      builtins.filter builtins.isString regexResult;

    slice = start: end: str:
      let
        len = builtins.stringLength str;
        positiveStart = if start >= 0 then start else len + start;
        positiveEnd = if end >= 0 then end else len + end;
      in
      if positiveEnd > positiveStart then
        builtins.substring positiveStart (positiveEnd - positiveStart) str
      else if positiveEnd == positiveStart then
        ""
      else # Reversed
        let
          len' = positiveStart - positiveEnd;
          tailIx = positiveStart - 1;
          newList = builtins.genList (n: builtins.substring (tailIx - n) 1 str) len';
        in
        builtins.concatStringsSep "" newList;

    left = n: prev: builtins.substring 0 n prev;
    right = n: prev:
      let len = builtins.stringLength prev;
      in
      builtins.substring (len - n) n prev;

    dropLeft = n: prev:
      let len = builtins.stringLength prev;
      in
      builtins.substring n (len - n) prev;
    dropRight = n: prev:
      let len = builtins.stringLength prev;
      in
      builtins.substring 0 (len - n) prev;

    contains = pattern: str:
      let
        safePattern = escape ssot.regexSpecialCharacters pattern;
        regexResult = builtins.split "(${safePattern})" str;
      in
      builtins.length regexResult > 1;

    startsWith = pattern: str:
      builtins.substring 0 (builtins.stringLength pattern) str == pattern;

    endsWith = pattern: str:
      let
        patternLength = builtins.stringLength pattern;
        strLength = builtins.stringLength str;
      in
      builtins.substring (strLength - patternLength) patternLength str == pattern;

    indexes = pattern: str:
      let
        patternLength = builtins.stringLength pattern;
        safePattern = escape ssot.regexSpecialCharacters pattern;
        regexResult = builtins.split "(${safePattern})" str;
        searchResult =
          builtins.foldl'
            ({ count, indexes }: item:
              if builtins.isString item then
                { count = count + builtins.stringLength item; inherit indexes; }
              else
                { count = count + patternLength; indexes = indexes ++ [ count ]; }
            )
            { count = 0; indexes = [ ]; }
            regexResult;
      in
      searchResult.indexes;

    toInt = str:
      let
        primitive = builtins.fromJSON str;
      in
      if builtins.isInt primitive then primitive
      else throw "${str} is not an integer.";

    fromInt = primitive:
      if builtins.isInt primitive then builtins.toJSON primitive
      else throw "${builtins.typeOf primitive} is not an integer.";

    toFloat = str:
      let
        primitive = builtins.fromJSON str;
      in
      if builtins.isFloat primitive || builtins.isInt primitive then primitive
      else throw "${str} is not a float.";

    fromFloat = primitive:
      if builtins.isFloat primitive then builtins.toJSON primitive
      else throw "${builtins.typeOf primitive} is not a float.";

    toUpper = prev: builtins.replaceStrings ssot.asciiLowerChars ssot.asciiUpperChars prev;
    toLower = prev: builtins.replaceStrings ssot.asciiUpperChars ssot.asciiLowerChars prev;

    padLeft = n: filler: prev:
      let len = builtins.stringLength prev;
      in
      if builtins.stringLength filler != 1 then
        throw "Invalid filler, wrong number of characters, should have one."
      else if len < n then repeat (n - len) filler + prev
      else prev;

    padRight = n: filler: prev:
      let len = builtins.stringLength prev;
      in
      if builtins.stringLength filler != 1 then
        throw "Invalid filler, wrong number of characters, should have one."
      else if len < n then prev + repeat (n - len) filler
      else prev;

    trimLeft = prev:
      let
        regexResult = builtins.split "([[:space:]]+)" prev;
        firstGroup = builtins.elemAt regexResult 1;
      in
      if builtins.length regexResult > 1 &&
        builtins.head regexResult == "" &&
        builtins.isList firstGroup then
        dropLeft (builtins.stringLength (builtins.head firstGroup)) prev
      else prev;

    trimRight = prev:
      let
        regexResult = builtins.split "([[:space:]]+)" prev;
        resultNum = builtins.length regexResult;
        lastGroup = builtins.elemAt regexResult (resultNum - 2);
      in
      if resultNum > 1 &&
        list.last regexResult == "" &&
        builtins.isList lastGroup then
        dropRight (builtins.stringLength (builtins.head lastGroup)) prev
      else prev;

    trim = prev:
      let
        regexResult = builtins.split "([[:space:]]+)" prev;
        resultNum = builtins.length regexResult;
        firstGroup = builtins.elemAt regexResult 1;
        lastGroup = builtins.elemAt regexResult (resultNum - 2);

        toDropLeft =
          if resultNum > 1 &&
            builtins.head regexResult == "" &&
            builtins.isList firstGroup then
            builtins.stringLength (builtins.head firstGroup)
          else 0;

        toDropRight =
          if resultNum > 1 &&
            list.last regexResult == "" &&
            builtins.isList lastGroup then
            builtins.stringLength (builtins.head lastGroup)
          else 0;
      in
      builtins.substring toDropLeft
        (builtins.stringLength prev - toDropLeft - toDropRight)
        prev;

    toHexMD5 = builtins.hashString "md5";
    toHexSHA1 = builtins.hashString "sha1";
    toHexSHA256 = builtins.hashString "sha256";
    toHexSHA512 = builtins.hashString "sha512";
  };

  ssot = {
    # taken from nixpkgs
    regexSpecialCharacters = string.toList "\\[{()^$?*+|.";
    asciiLowerChars = string.toList "abcdefghijklmnopqrstuvwxyz";
    asciiUpperChars = string.toList "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  };

  type = {
    inherit (builtins)
      isAttrs
      isBool
      isFloat
      isFunction
      isInt
      isList
      isNull
      isPath
      isString;
    of = x: builtins.typeOf x;
  };

  eval = {
    inherit (builtins)
      abort
      deepSeq
      functionArgs
      seq
      trace;
    try = builtins.tryEval;
  };

  drv = {
    inherit (builtins)
      filterSource
      parseDrvName
      placeholder
      storePath;
  };

  codecs = {
    inherit (builtins)
      fromJSON
      toFile
      toJSON
      toPath
      toString
      toXML;
  };

  io = {
    inherit (builtins)
      fetchClosure
      fetchGit
      fetchTarball
      readDir
      readFile
      import
      pathExists;
    fileHexMD5 = builtins.hashFile "md5";
    fileHexSHA1 = builtins.hashFile "sha1";
    fileHexSHA256 = builtins.hashFile "sha256";
    fileHexSHA512 = builtins.hashFile "sha512";
    importFlake = builtins.getFlake;
    getEnv = builtins.getEnv;
    fetchURL = builtins.fetchurl;
  };

  path = {
    inherit (builtins)
      baseNameOf
      dirOf;
    build = builtins.path;
  };

  semVer = {
    compare = builtins.compareVersions;
    fromString = builtins.splitVersion;
  };

  regex = {
    inherit (builtins)
      match
      split;
  };
}
