# TensorXport
Module written for xAct with two main functionalities:
- `ToGRTensor`: Translate a xAct expression into GRTensor Maple string
- `IntegrateByParts`: Integrate by parts terms quadratic in the perturbation (`hh`) with covariant derivatives (`CD`) acting on both factors

Note: The names for the covariant derivative and the perturbation are hardcoded, so please use them.

### Installation
Load the TensorXport.wl with the function `Get` from Mathematica.
### Usage of ToGRTensor
Let `expr` be a xAct then
```
ToGRTensor[expr]
```
will return a string that you can copy to use it in GRTensor.

If the expression has many lines, it is recommended to use:
1. Select the output
2. Right- click -> Copy As -> Plain Text
### Usage of IntegrateByParts
Let `M` be the manifold where you are working. 
1. Declare a vector that will be considered as the normal vector to the boundary.
```
DefTensor[norm[a], M]
```
3. Define the boundary context.
```
boundyContext = SetNormalVector[norm, M];
```
5. Let `expr` be a xAct expression then to integrate by parts do
```
IntegrateByParts[boundyContext][expr]
```
6. For simplicity you can define a function that knows the boundary context:
```
intbyparts[X_] := IntegrateByParts[boundyContext][X]
```
