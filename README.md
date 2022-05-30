<!-- # marray -->
<h2>Overview marray</h2>
Multidimensional Array

This R library is a replica of ndarray and functionality from NumPy.

<ul>
  <li><code>ndim()</code> returns the number of dimensions of an array.</li>
  <li><code>nsize()</code> returns the number of elements of an array.</li>
  <li><code>DIM()</code> returns the dimension of an object or its length.</li>
  <li><code>dimC()</code> set the dimension of an object in row-major ordering (C-style).</li>
  <li><code>reshape.array()</code> reshapes an array to new dimension.</li>
  <li><code>marray()</code> and <code>as.marray()</code> transform data into a multidimensional array and <code>is.marray()</code> checks for that type of array.</li>
  <li><code>flatten()</code> flattens data into a one-dimensional array.</li>
  <li><code>expand_dims()</code> expands the shape of an array by inserting a new axis.</li>
  <li><code>squeeze()</code> removes dimensions of length one from array.</li>
  <li><code>mamatrix()</code> shrinks an array by rows or columns into a matrix.</li>
  <li><code>slice()</code> read or write a part of an array.</li>
  <li><code>mabind()</code> combines input arrays to an output array along a specified axis.</li>
  <li><code>column_stack()</code> stacks 1D arrays as columns into a 2D array.</li>
  <li><code>row_stack()</code> stacks 1D arrays as rows into a 2D array.</li>
  <li><code>eye()</code> creates a 2D identity matrix.</li>
  <li><code>vander()</code> creates a Vandermonde matrix.</li>
  <li><code>ones()</code> creates an array filled with ones.</li>
  <li><code>zeros()</code> creates an array filled with zeros.</li>
  <li><code>empty()</code> creates an array filled with NA.</li>
  <li><code>full()</code> creates an array filled with value.</li>
  <li><code>insert()</code> inserts objects into an array.</li>
  <li><code>delete()</code> deletes axes from an array.</li>
  <li><code>transpose()</code> transposes an array by swapping dimensions.</li>
  <li><code>rearrange()</code> rearranges axis-driven an array by swapping dimensions.</li>
  <li><code>flip()</code> reverses the order of the elements of an array along axes.</li>
  <li><code>rot90()</code> rotates an array by 90 degrees in the plane specified by axes.</li>
  <li><code>embedseries()</code> resamples data into an ongoing shifted series array.</li>
</ul>
