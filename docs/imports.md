# Import System

## Case Sensitivity
All import paths are case-sensitive. Ensure that the case used in import statements matches exactly with the file and directory names.

## Syntax
Imports can be declared two ways:

```py
from path import name
# or
import path/name
```
## Placement
Imports should be placed at the top of a file.

## Project Structure
Let's assume we have a bend project with the following structure:

```
my_project/
├── main.bend
├── utils/
│   ├── helper.bend
│   │   └── def calc
│   └── math.bend
│       ├── def add
│       └── def subtract
```

## Importing Relative Paths
Paths starting with `./` or `../` are imported relative to the file.

### Example:
```py
# if used inside `my_project/*.bend`
from ./utils import helper
# if used inside `my_project/*/*.bend`
import ../utils/helper
```

This will bind `calc` from `helper.bend` as `helper/calc`.

## Importing Absolute Paths
Otherwise, paths imported are relative to the folder of the main file.

### Example:
```py
from utils import math
# or
import utils/math
```

This will bind `add` and `subtract` from `math.bend` as `math/add` and `math/subtract`.

## Importing Specific Top-Level Names
You can import specific top-level names from a file.

### Example:
```py
from utils/helper import calc
from utils/math import (add, subtract)
# or
import (utils/helper/calc, utils/math/add, utils/math/subtract)
# or
import utils/helper/calc
import utils/math/add
import utils/math/subtract
```

This will bind the names `calc`, `add` and `subtract` from their respective files. 

## Importing All Names
You can import all top-level names from a file using the wildcard `*`.

### Example:
```py
from utils/math import *
```

This will bind the names `add` and `subtract` from `math.bend`. 

## Importing All `.bend` Files from a Folder
You can import all `.bend` files from a folder using the wildcard `*`.

### Example:
```py
from utils import *
```

This will bind the names from `helper.bend` and `math.bend`, as `helper/calc`, `math/add` and `math/subtract`.

## Aliasing Imports
You can alias imports to a different name for convenience.

### Importing a File with Alias
Import the `file` top-level name from `file.bend` aliased to `alias`, and all other names as `alias/name`.

### Example:
```py
from utils import helper as utilsHelper
import utils/math as mathLib
```

This will bind the names from `helper.bend` and `math.bend`, as `utilsHelper/calc`, `mathLib/add` and `mathLib/subtract`.

### Importing Specific Names with Aliases
You can import specific top-level names and alias them to different names.

### Example:
```py
from utils/helper import calc as calcFunc
from utils/math import (add as addFunc, subtract as subFunc)
# or
import (utils/math/add as addFunc, utils/math/subtract as subFunc)
```

This will bind `calc`, `add` and `subtract` as `calcFunc`, `addFunc` and `subFunc` from their respective files.

## Project Structure
Let's assume we have a bend project with the following structure:

```
my_project/
├── main.bend
├── types/
│   ├── List.bend
│   │   └── type List: Nil | (Cons ..)
│   └── List/
│       ├── concat.bend
│       │   └── def concat
│       └── append.bend
│           └── def append
│           └── def helper
```

## Importing data types

You can import a data type and its constructors by only importing its name.

### Example:
```py
from types/List import List
# behaves the same as 
from types/List import (List, List/Nil, List/Cons)
```

Importing only `List` from `List.bend` will import the type `List` and bind its constructors name `List/Nil` and `List/Cons`. 

## Importing files with a top level name equal to its name

When a file and a top-level name in it share a name, for example, `List.bend`, `concat.bend` and `append.bend`, the bind of that import is simplified to the file name.

### Example:
```py
from types/List import append
```

This will bind `append` and `append/helper` from `append.bend`.

## Files and directories with the same name

When files and directories share a name, both share the import namespace:

```py
from types/List import (List, concat)
```

This will attempt to import from both the `List.bend` file and the `List` folder, resulting in the binds `List/Nil`, `List/Cons` and `concat`.

```py
from types/List import *
```

This will import all the names from `List.bend`, then all the files inside the `List` folder, resulting in the binds `List/Nil`, `List/Cons`, `concat`, `append` and `append/helper`.

In both cases, if a name is present as a top-level name on the file, and as a `.bend` file name inside the folder, it will result in an error.

If you only want to import `List.bend` and not search the files in its folder, you can use the `import path` syntax:

```py
import types/List
```
