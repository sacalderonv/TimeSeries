Estructural
================

Modelos Estructurales
---------------------

En la presente sección, vamos a introducir una metodología para el
análisis de series de tiempo vía modelos estructurales a través de los
modelos de Espacio-Estado. Por ésta razón, es necesario tener el
pre-requisito de los modelos de espacio-estado y el filtro de Kalman, el
libro base será el libro de Durbin y Koopman. Consideraremos el
siguiente modelo estructural para la serie de tiempo {*Y*<sub>*t*</sub>}

$Y\_{t}=\\mu\_{t}+\\gamma\_{t}+c\_{t}+\\sum\_{j=1}^{k}\\beta\_{j}x\_{jt}+\\varepsilon\_{t}$
donde *μ*<sub>*t*</sub> es una componente de variación lenta llamada ,
*γ*<sub>*t*</sub> es una componente periódica de periodo fijo llamada ,
*c*<sub>*t*</sub> es la componente cíclica de periodo mayor a la
componente estacional, *x*<sub>*j**t*</sub> es la *j*−ésima variable
regresora o explicativa, y *ε*<sub>*t*</sub> es la componente irregular
o de error.

Veamos una primera propuesta de modelo para cada una de las componentes.
La propuesta La componente de tendencia *μ*<sub>*t*</sub> se puede ver
como una extensión dinámica del modelo de regresión de intercepto y
pendiente:
*μ*<sub>*t* + 1</sub> = *μ*<sub>*t*</sub> + *ν*<sub>*t*</sub> + *η*<sub>*t* + 1</sub>,    *η*<sub>*t* + 1</sub> ∼ *N*(0, *σ*<sub>*η*</sub><sup>2</sup>)
*ν*<sub>*t* + 1</sub> = *ν*<sub>*t*</sub> + *ζ*<sub>*t* + 1</sub>,    *ζ*<sub>*t* + 1</sub> ∼ *N*(0, *σ*<sub>*ζ*</sub><sup>2</sup>)

La componente estacional *γ*<sub>*t*</sub> puede ser escrita como:
$$\\gamma\_{t}=-\\sum\_{j=1}^{s-1}\\gamma\_{t+1-j}+\\omega\_{t},\\  \\  \\  \\omega\_{t}\\sim N(0,\\sigma^{2}\_{\\omega})$$
La condición principal es que la la suma de las componentes estacionales
en un ciclo completo es cero. \\

La componente cíclica trata de capturar los efectos cíclicos en etapas
de tiempo mas grandes que las capturadas por la componente estacional.
Para el caso de ciclos económicos, ellos intentan capturar los ciclos de
negocios los cuales se esperan que tengan un periodo entre 1.5 y 12
años, es decir 2*π*/*λ*<sub>*c*</sub>.\\ La componente cíclica
determinística puede ser escrita como
$$c\_t=\\widetilde{c} cos\\lambda\_ct+ \\widetilde{c^\*}sin\\lambda\_c t $$
Mientras que la componente cíclica también puede considerar se de la
siguiente manera con 0 &lt; *ρ*<sub>*c*</sub> ≤ 1:
*c*<sub>*t* + 1</sub> = *ρ*<sub>*c*</sub>\[*c*<sub>*t*</sub>cos *λ*<sub>*c*</sub> + *c*<sub>*t*</sub><sup>\*</sup>sin *λ*<sub>*c*</sub>\] + *ω̃*<sub>*t*</sub>,   *ω̃*<sub>*t*</sub> ∼ *N*(0, *σ*<sub>*ω̃*</sub><sup>2</sup>)
*c*<sub>*t* + 1</sub><sup>\*</sup> = *ρ*<sub>*c*</sub>\[ − *c*<sub>*t*</sub>sin *λ*<sub>*c*</sub> + *c*<sub>*t*</sub><sup>\*</sup>sin *λ*<sub>*c*</sub>\] + *ω̃*<sub>*t*</sub><sup>\*</sup>,   *ω̃*<sub>*t*</sub><sup>\*</sup> ∼ *N*(0, *σ*<sub>*ω̃*<sup>\*</sup></sub><sup>2</sup>)
Se puede observar que *λ*<sub>*c*</sub> es la frecuencia del ciclo, la
cual pasa a ser un parámetro del modelo o identificado por el
periodograma.\\ Los modelos presentados anteriormente para las
diferentes componentes no son lo únicos.
