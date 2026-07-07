# MATLAB numerical reference fixtures

These fixtures validate the R demographic core against saved outputs from the
original `Model-V2022` MATLAB workflow. They are deliberately self-contained:
the numerical vectors are stored in `helper-matlab-reference-cases.R`, so that
`R CMD check` does not depend on external Excel files or MATLAB.

## Provenance

**Input workbook**

```text
Model-V2022/data-and-code/1Especie/Datos(Preys).xlsx
```

**MATLAB result workbooks**

```text
Model-V2022/data-and-code/1Especie/Resultados/Resultados_C. fiber.xlsx
Model-V2022/data-and-code/1Especie/Resultados/Resultados_C. elaphus.xlsx
```

**MATLAB code route**

```text
G1.m -> G2.m -> BuscalambdaEstable.m -> CalculaFuncionEstabilidadLambda.m
     -> wblcdf.m
```

The tests use the saved MATLAB values for the Weibull scale (`lambda`, named
`alpha` in R), survivorship (`X`/`lx`), stable structure (`R`), mortality exit
profile (`D`) and conditional survival (`B`). They also verify the historical
C. fiber terminal window `0.0001 <= X_final <= 0.05`.

## Tolerance

A numerical tolerance of `1e-8` is used. It allows harmless differences between
MATLAB `fzero()` and R `uniroot()` while remaining strict enough to detect a
change in the demographic formulas, class indexing, or terminal filtering.

## Actualización de referencias

Las referencias proceden de las salidas MATLAB conservadas, no de resultados
recalculados por R. Si se actualiza el modelo de origen o se decide usar otro
conjunto de casos de referencia, hay que documentar la procedencia, guardar los
valores esperados y revisar explícitamente la tolerancia antes de modificar las
pruebas.
