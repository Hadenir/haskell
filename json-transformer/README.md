# json-transformer

Tool for transforming json files using defined set of rules.

## Usage

```bash
jsot <input_file.json> <rules_file.txt> <output_file.json>
```

## Transformation Rules

### JSON Path

Rules file uses JSON paths to specify which field to transform.
Each path must begin with 'root', which is represented by dollar `$` sign.
Object members are referenced by their name preceded with a dot, `.member`.
Array elements are specified by their index inside quare brackets, `[0]`.
Whole array (each of its elements) may be described using `[*]`.

Example valid paths:
```
$.foo.bar
$.foo[0].bar
$[0].foo.bar
```

### Transformations

Following transformation rules are implemented:

* setting value to null: `$.a = null`
* setting value to constant boolean: `$.b = true`
* setting value to constant string: `$.c = "\"foo\\bar\""`
* setting value to constant number: `$.d = 123.45e-2`
* removing value: `$.e.f = ~`
* transformation of whole array: `$g[*].h = null`
