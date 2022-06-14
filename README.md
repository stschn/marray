<!-- # marray -->
<h2>Overview marray</h2>
Multidimensional Array

This R library is a replica of ndarray and functionality from NumPy.

<ul>
  <li><code>ndim()</code> returns the number of dimensions of an array.</li>
  <li><code>nsize()</code> returns the number of elements of an array.</li>
  <li><code>DIM()</code> returns the dimension of an object or its length.</li>
  <li><code>ensuredim()</code> and <code>dropdim()</code> enforces an array or a vector.</li>
  <li><code>dimC()</code> set the dimension of an object in row-major ordering (C-style).</li>
  <li><code>reshape.array()</code> reshapes an array to new dimension.</li>
  <li><code>marray()</code> and <code>as.marray()</code> transform data into a multidimensional array and <code>is.marray()</code> checks for that type of array.</li>
  <li><code>atleast_1d()</code>, <code>atleast_2d()</code> and <code>atleast_3d()</code> coerces objects into arrays with corresponding number of dimensions.</li>
  <li><code>flatten()</code> flattens data into a one-dimensional array.</li>
  <li><code>expand_dims()</code> expands the shape of an array by inserting a new axis.</li>
  <li><code>squeeze()</code> removes dimensions of length one from array.</li>
  <li><code>mamatrix()</code> shrinks an array by rows or columns into a matrix.</li>
  <li><code>slice()</code> read or write a part of an array.</li>
  <li><code>maclip()</code> limits values of an array.</li>
  <li><code>mabind()</code> combines input arrays to an output array along a specified axis.</li>
  <li><code>vstack()</code> stacks arrays in sequence vertically (row-wise).</li>
  <li><code>hstack()</code> stacks arrays in sequence horizontally (column-wise).</li>
  <li><code>dstack()</code> stacks arrays in sequence along 3rd axis (depth-wise)).</li>
  <li><code>column_stack()</code> stacks 1D arrays as columns into a 2D array.</li>
  <li><code>row_stack()</code> stacks 1D arrays as rows into a 2D array.</li>
  <li><code>array_split()</code> splits an array into sub-arrays along an axis.</li>
  <li><code>vsplit()</code> splits an array into sub-arrays vertically (row-wise).</li>
  <li><code>hsplit()</code> splits an array into sub-arrays horizontally (column-wise).</li>
  <li><code>dsplit()</code> splits an array into sub-arrays along 3rd axis (depth-wise)).</li>
  <li><code>eye()</code> creates a 2D identity matrix.</li>
  <li><code>vander()</code> creates a Vandermonde matrix.</li>
  <li><code>ones()</code> creates an array filled with ones.</li>
  <li><code>zeros()</code> creates an array filled with zeros.</li>
  <li><code>empty()</code> creates an array filled with NA.</li>
  <li><code>full()</code> creates an array filled with value.</li>
  <li><code>maidentity()</code> creates an identity array.</li>
  <li><code>insert()</code> inserts objects into an array.</li>
  <li><code>delete()</code> deletes axes from an array.</li>
  <li><code>transpose()</code> transposes an array by swapping dimensions.</li>
  <li><code>rearrange()</code> rearranges axis-driven an array by swapping dimensions.</li>
  <li><code>flip()</code> reverses the order of the elements of an array along axes.</li>
  <li><code>flipud()</code> flips an array vertically (axis = 1).</li>
  <li><code>fliplr()</code> flips an array horizontally (axis = 2).</li>
  <li><code>rot90()</code> rotates an array by 90 degrees in the plane specified by axes.</li>
  <li><code>roll()</code> shifts an array circularly.</li>
  <li><code>crop()</code> takes out a part of an array with default values for the remaining part.</li>
  <li><code>slide()</code> slides over an array with a window of given size and given stride.</li>
  <li><code>embedseries()</code> resamples data into an ongoing shifted series array.</li>
  <li><code>mareplace()</code> replaces values in an array.</li>
</ul>

<h2>Examples</h3>
<p><code>library(marray)</code></p>

<h3>Array creation from scratch</h3>

```r
# Filled with NA
a <- empty(dim = c(4, 3, 2))
# Filled with Zeros
a <- zeros(dim = c(4, 3, 2))
# Filled with Ones
a <- ones(dim = c(4, 3, 2))
# Filled with value(s)
a <- full(dim = c(4, 3, 2), fill_value = 11)
a <- full(dim = c(4, 3, 2), fill_value = c(11, 2, 23), order = "F")
```

<h3>Array creation with ordering and from other data</h3>

```r
# Row-major ordering
a <- marray(1:24, dim = c(4, 3, 2))
# Column-major ordering
a <- marray(1:24, dim = c(4, 3, 2), order = "F")

# Different types of data
v <- (1:24)
l <- list(x1 = 1:10, x2 = seq(10, 100, 10))
df <- data.frame(x1 = 1:6, x2 = seq(10, 60, 10), x3 = sample(letters, 6))
m <- matrix(1:24, nrow = 6)
a1 <- array(letters[1L:24L])
a3 <- array(v, dim = c(4, 3, 2))
a4 <- array(1:48, dim = c(4, 3, 2, 2))

a <- marray(v) # just change the argument
```

<h3>Expand and squeeze dimensions</h3>

```r
a <- marray(seq_len(12))
a <- expand_dims(a)
a <- squeeze(a)
```

<h3>Flatten data</h3>

```r
a4 <- array(1:48, dim = c(4, 3, 2, 2))
flatten(a4, order = "F")
```

<h3>Bind arrays in pipe-friendly way</h3>

```r
x <- marray(seq_len(24), dim = c(4, 3, 2), order = "F")
y <- marray(-seq_len(24), dim = c(4, 3, 2), order = "F")
z <- marray(seq_len(12), dim = c(4, 3))

a <- expand_dims(z) |>
  list() |>
  append(list(x, y), 0L) |>
  mabind()
```

<h3>Insert into array</h3>

```r
a <- marray(seq.int(2 * 3 * 4), dim = c(2, 3, 4), order = "F")
x <- marray(100L + seq.int(2 * 1 * 4))
insert(a, x, axis = 2L, order = "F") # x will automatically be coerced in the right shape
```

<h3>Read and write slices of an array</h3>

```r
a <- marray(1:48, dim = c(4, 3, 2, 2))
slice(a) # read complete four-dimensional array
slice(a, l = 2) # the values of the second element of the last dimension (4th dimension)
slice(a, i = 1, j = 3) # the values of the first element of the first dimension (1st row) and the third element of the second dimension (3rd column) across all bunches of the remaining dimensions 3 and 4.

a <- marray(1:24, dim = c(4, 3, 2), order = "F")
slice(a, i = 1L) <- 0L; a # write 0 to the first dimension (row) across all remaining dimensions
slice(a, i = 1L) <- 100:102; a # write 100-102 to the first dimension (row) across all remaining dimensions
slice(a, i = 1L) <- 100:105; a # write 100-105 to the first dimension (row) across all remaining dimensions
slice(a, i = 1L) <- matrix(100:105, nrow = 3L); a # equal to prior, nrow can be 1, 2, 3, or 6
```

<h3>Flip array</h3>

```r
a <- marray(seq_len(24), dim = c(4, 3, 2), order = "F")
# Along first dimension
flip(a)
# Along second dimension
flip(a, axis = 2L)
# Along third dimension
flip(a, axis = 3L)
# Along first and second dimension
flip(a, axis = c(1L, 2L))
```

<h3>Rotate array</h3>

```r
a <- marray(seq_len(12), dim = c(4, 3), order = "F")
# Clockwise
rot90(a)
# Two times clockwise
rot90(a, k = 2L)
# Counterclockwise
rot90(a, k = -1L)
# Three times counterclockwise
rot90(a, k = -3L)
```
<h3>Embed series</h3>

```r
a <- marray(1:24, dim = c(6, 4), order = "F")
embedseries(a, length = 3L)
```

<h3>Roll array</h3>

```r
a <- marray(0:9)
roll(a, shift = 2)
roll(a, shift = -2)
a <- marray(a, dim = c(2, 5))
roll(a, shift = 1)
roll(a, shift = -1)
roll(a, shift = 1, axis = 1)
roll(a, shift = -1, axis = 1)
```

<h3>Crop array</h3>

```r
a <- marray(1:24, dim = c(4, 3, 2))
crop(a, i = 1, j = 2:3)
```

<h3>Split array</h3>

```r
a <- marray(1:8)
sub_arys <- array_split(a, indices_or_sections = 3)
sub_arys <- array_split(a, indices_or_sections = c(3, 5, 6))

a <- marray(1:12, dim = c(4, 3))
sub_arys <- array_split(a, indices_or_sections = 2)
sub_arys <- array_split(a, indices_or_sections = 2, axis = 2L)

a <- marray(1:24, dim = c(4, 3, 2), order = "F")
sub_arys <- array_split(a, indices_or_sections = 2)
sub_arys <- array_split(a, indices_or_sections = 2, axis = 2L)
```

<h3>Slide over an array</h3>

```r
a <- marray(sample(7 * 4 * 2), dim = c(7, 4, 2))
# Sliding along each axes with a size of 1 and a stride of 1
sub_arys <- slide(a)
# Sliding along axes 1 and 2 with a size of 2 resp. 1 and a stride of 1
sub_arys <- slide(a, size = c(2, 1), axis = c(1, 2))
```
