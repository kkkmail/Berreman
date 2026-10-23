# Experimental protocol represented by the primary `MuellerInverseTests` test

## 1. Purpose and scope

The primary test, `the full C1-C4 measurement set recovers every material constant from a perturbed start`, represents a single-wavelength Mueller-polarimetry experiment on a transparent, homogeneous, optically active uniaxial crystal. Its inverse problem determines four quantities:

- the ordinary refractive index, `n_o`;
- the extraordinary refractive index, `n_e`;
- the two independent optical-activity parameters named `g11` and `g33` by the Berreman model.

The sample thickness, crystal cut, sample azimuth, and wavelength are known experimental inputs and are not fitted. The test uses right-handed alpha-quartz as the synthetic material and fixes the wavelength at 632.8 nm.

The test is a noiseless numerical round trip. It generates every “measured” Mueller matrix with the same Berreman forward model that is later fitted. It therefore verifies the inverse algorithm and the sufficiency of the configuration set; it is not, by itself, a validation against measurements of real quartz. Exact recovery at the test tolerances can be reproduced only with the synthetic data. A physical realization can reproduce the configuration set and the inversion procedure, but its acceptance criteria must account for measurement uncertainty and model discrepancy.

## 2. Samples and optical arrangement

Three free-standing, plane-parallel crystal plates in air are needed:

1. a 1.0 mm z-cut plate, with the optic axis parallel to the surface normal;
2. a 20 µm z-cut plate, with the optic axis parallel to the surface normal;
3. a 20 µm x-cut plate, with the optic axis in the surface plane.

The thickness and cut of every plate must be known independently. The test assumes vacuum/air on both sides, no supporting substrate, no coatings, no additional films, no absorption, and no scattering. The plates must be mounted so that both the external angle of incidence and rotation about the surface normal are known. Zero sample azimuth must be defined relative to the fixed plane of incidence; increasing azimuth means rotating the crystal about its surface normal.

Use monochromatic light at 632.8 nm, corresponding to a He-Ne wavelength. The source, polarization-state generator, polarization-state analyzer, and detector must form a calibrated full Mueller polarimeter. The required observable at each configuration is a complete 4 × 4 Mueller matrix, not a single transmitted intensity. The test does not prescribe a particular polarizer/analyzer state sequence; any calibrated sequence with enough independent input and analyzer states to reconstruct all 16 matrix elements is acceptable. Transmission and reflection must use coordinate and handedness conventions consistent with those used by the forward model.

The model includes the direct beams and internal-reflection contributions selected by `numberOfReflections = 3`, and adds the corresponding Mueller matrices incoherently. A physical arrangement intended to match that model must collect the same beam family and suppress or average coherent Fabry-Pérot interference. If the internally reflected beams are spatially rejected, remain mutually coherent, or are collected differently in the two arms, the measured matrices describe a different forward problem and must not be fitted with the unchanged test model.

## 3. Required measurement configurations

All angles of incidence in the table are external angles in air, measured from the surface normal. Every transmission/reflection entry is one separate full Mueller-matrix observation.

| Set | Plate | Incidence angles | Sample azimuths | Mueller observables | Number of matrices | Experimental role |
|---|---|---:|---:|---|---:|---|
| C1 | z-cut, 1.0 mm | 0° | 0° | transmission | 1 | Determines the sign and magnitude of `g11` directly from pure optical rotation in the engine convention. |
| C2 | z-cut, 20 µm | 10°, 30°, 50°, 70° | 0°, 45° | transmission and reflection at every angle/azimuth pair | 16 | Determines the absolute levels of `n_o` and `n_e` from their angle-dependent transmission and reflection, while adding a weaker independent constraint on `g33`. |
| C3 | x-cut, 20 µm | 0° | 0°, 22.5°, 45°, 67.5° | transmission | 4 | Determines the birefringence `n_e - n_o`, makes `g33` directly observable, and, together with C1, separates `g33` from `g11`. |
| C4 | x-cut, 20 µm | 15°, 45° | 0°, 45° | transmission and reflection at every angle/azimuth pair | 8 | Refines `g33`, `n_o`, and `n_e` through oblique transmission and reflection and reduces the remaining correlations among the four fitted parameters. |

The complete data set therefore contains 29 matrices: 17 transmission matrices and 12 reflection matrices.

C1 alone is insufficient. For propagation along the optic axis, `n_e` and `g33` have exactly no effect in this model, while `n_o` enters only very weakly through the relative weight of internally reflected beams. C3 supplies the second crystal cut that restores sensitivity to all four parameters. C2 and C4 then improve the determination of the common, absolute level of `n_o` and `n_e` through oblique-incidence Fresnel amplitudes and internal refraction angles. Tilting a z-cut specimen cannot replace the x-cut specimen because Snell’s law limits the internal propagation angle to about 40.4° for quartz.

The 20 µm thickness is deliberate. It keeps the linear retardance below one wave and avoids the many local minima that a 1 mm birefringent plate would create. The 1.0 mm plate is used only for C1, where normal propagation along the optic axis produces no linear retardance and the longer path increases optical rotation.

## 4. Acquisition procedure

1. Calibrate the polarimeter at 632.8 nm in both transmission and reflection, including dark signals, reference throughput, angular zero, sample-azimuth zero, and the Stokes-frame convention of the reflected beam. A known retarder or rotator should be used to verify the signs of the off-diagonal Mueller elements.
2. Mount the 1.0 mm z-cut plate and acquire the C1 transmitted Mueller matrix at normal incidence and 0° azimuth.
3. Mount the 20 µm z-cut plate. At each of the four incidence angles, acquire transmission and reflection matrices at both 0° and 45° azimuth.
4. Mount the 20 µm x-cut plate. At normal incidence, acquire transmitted matrices at 0°, 22.5°, 45°, and 67.5° azimuth.
5. With the same x-cut plate, acquire transmission and reflection matrices at all four combinations of incidence angle 15° or 45° and azimuth 0° or 45°.
6. Store each matrix together with its wavelength, cut, independently measured thickness, incidence angle, azimuth, observable, handedness, and polarization-coordinate convention. The software contract requires one matrix per listed configuration. Physical repeats may be averaged, but repeats and their scatter must also be retained to estimate uncertainty.

## 5. Data reduction and inverse fit

For every measured matrix `M`, require a finite positive `m00`, divide all elements by `m00`, and omit the now-identical normalized `m00 = 1` from the residual. Each configuration contributes 15 dimensionless residuals, so the 29 configurations produce 435 residual entries. Absolute source power, detector gain, and absolute throughput are consequently not fitted.

Before fitting, check that the matrices are physically realizable by evaluating their Cloude coherency eigenvalues. Also inspect the depolarization index and the dichroism terms. The synthetic transmission matrices are nearly non-depolarizing; reflection is substantially more depolarizing because front-surface and internally reflected beams of comparable intensity are added incoherently. The closed-form Mueller-Jones inversion is therefore suitable only as a transmission diagnostic or source of starting values, not as the final answer for the reflection data.

Fit all 435 normalized elements simultaneously, with equal implicit weight, by nonlinear least squares against the complete Berreman plate model. The model material is uniaxial, with principal permittivity `diag(n_o², n_o², n_e²)` and two independent `Rho` components. The primary test uses Levenberg-Marquardt, scales refractive-index coordinates by `10^-3` and optical-activity coordinates by `10^-5`, permits ±0.05 around each starting refractive index and ±`5 × 10^-4` around each starting optical-activity value, and allows up to 400 iterations.

For the exact numerical round trip, the synthetic truth is:

| Parameter | Synthetic value |
|---|---:|
| `n_o` | 1.542606 |
| `n_e` | 1.551651 |
| `g11` | `5.9 × 10^-5` |
| `g33` | `-10.1 × 10^-5` |

The test starts from `n_o` 0.2% too high, `n_e` 0.1% too high, and both optical-activity components 30% away from the truth. With noiseless self-generated data, the required result is relative error below `10^-11` for every fitted parameter, total squared residual below `10^-18`, and worst normalized Mueller-element residual below `10^-9`. These thresholds are numerical regression thresholds, not laboratory specifications. Real measurements require uncertainty-weighted residuals, repeatability estimates, parameter confidence intervals, and tolerances derived from the calibrated instrument noise.

## 6. Mandatory parameter-convention warning

The quantities called `g11` and `g33` in this test are the Berreman engine’s bi-anisotropic magnetoelectric `Rho` parameters in the constitutive convention `D = εE + ρH`. They are not directly the crystallographic gyration tensor in the convention `D = εE + i(G × E)`, although the same symbols are commonly used for both.

In the engine convention, the C1 z-cut rotation is governed by the transverse component `g11` and follows `2π g11 d / λ`; the synthetic constants give about 33.57°/mm. Reading the quoted quartz constants in the crystallographic convention gives about 18.62°/mm, close to the measured rotatory power of real quartz. Consequently, a laboratory fit must report which convention it estimates and must perform an explicit, validated conversion before comparing the fitted `Rho` values with published crystallographic gyration values. Without that conversion, a real-quartz experiment cannot be expected to reproduce the synthetic `g11` and `g33` values as physical literature constants.

---

# Экспериментальный протокол, представленный основным тестом `MuellerInverseTests`

## 1. Цель и область применения

Основной тест `the full C1-C4 measurement set recovers every material constant from a perturbed start` представляет одночастотный эксперимент по мюллеровской поляриметрии прозрачного, однородного, оптически активного одноосного кристалла. В обратной задаче определяются четыре величины:

- обыкновенный показатель преломления `n_o`;
- необыкновенный показатель преломления `n_e`;
- два независимых параметра оптической активности, обозначенные в модели Берремана как `g11` и `g33`.

Толщина образца, срез кристалла, азимут образца и длина волны являются известными входными данными эксперимента и не подгоняются. В тесте в качестве синтетического материала используется правовращающий альфа-кварц, а длина волны зафиксирована на 632,8 нм.

Тест является безошибочным численным замкнутым циклом. Каждая «измеренная» матрица Мюллера генерируется той же прямой моделью Берремана, которая затем подгоняется. Поэтому тест проверяет обратный алгоритм и достаточность набора конфигураций, но сам по себе не является проверкой по измерениям реального кварца. Точное восстановление с допусками теста можно воспроизвести только на синтетических данных. Физическая реализация может воспроизвести набор конфигураций и процедуру решения обратной задачи, но ее критерии приемки должны учитывать погрешность измерений и расхождение модели с реальностью.

## 2. Образцы и оптическая схема

Необходимы три свободно стоящие плоскопараллельные кристаллические пластины в воздухе:

1. пластина z-среза толщиной 1,0 мм, оптическая ось которой параллельна нормали к поверхности;
2. пластина z-среза толщиной 20 мкм, оптическая ось которой параллельна нормали к поверхности;
3. пластина x-среза толщиной 20 мкм, оптическая ось которой лежит в плоскости поверхности.

Толщина и срез каждой пластины должны быть известны независимо. В тесте предполагаются вакуум/воздух с обеих сторон, отсутствие несущей подложки, покрытий и дополнительных пленок, а также отсутствие поглощения и рассеяния. Пластины должны быть установлены так, чтобы были известны как внешний угол падения, так и поворот вокруг нормали к поверхности. Нулевой азимут образца должен быть определен относительно фиксированной плоскости падения; увеличение азимута означает поворот кристалла вокруг нормали к его поверхности.

Следует использовать монохроматический свет с длиной волны 632,8 нм, соответствующей длине волны He-Ne-лазера. Источник, генератор поляризационных состояний, анализатор поляризационных состояний и детектор должны образовывать откалиброванный полный мюллеровский поляриметр. Требуемой наблюдаемой величиной в каждой конфигурации является полная матрица Мюллера 4 × 4, а не одна прошедшая интенсивность. Тест не задает конкретную последовательность состояний поляризатора и анализатора; допустима любая откалиброванная последовательность с достаточным числом независимых входных и анализирующих состояний для восстановления всех 16 элементов матрицы. Для пропускания и отражения должны использоваться системы координат и соглашения о знаках, согласованные с прямой моделью.

Модель включает прямые пучки и вклады внутренних отражений, выбранные параметром `numberOfReflections = 3`, и некогерентно складывает соответствующие матрицы Мюллера. Физическая схема, предназначенная для соответствия этой модели, должна собирать то же семейство пучков и подавлять или усреднять когерентную интерференцию Фабри–Перо. Если внутренне отраженные пучки пространственно отсекаются, остаются взаимно когерентными или по-разному собираются в двух каналах, измеренные матрицы описывают другую прямую задачу и не должны подгоняться неизмененной моделью теста.

## 3. Требуемые конфигурации измерений

Все углы падения в таблице являются внешними углами в воздухе и отсчитываются от нормали к поверхности. Каждая запись «пропускание/отражение» является отдельным наблюдением полной матрицы Мюллера.

| Набор | Пластина | Углы падения | Азимуты образца | Наблюдаемые матрицы Мюллера | Число матриц | Роль в эксперименте |
|---|---|---:|---:|---|---:|---|
| C1 | z-срез, 1,0 мм | 0° | 0° | пропускание | 1 | Непосредственно определяет знак и величину `g11` по чистому оптическому вращению в соглашении движка. |
| C2 | z-срез, 20 мкм | 10°, 30°, 50°, 70° | 0°, 45° | пропускание и отражение для каждой пары угол/азимут | 16 | Определяет абсолютные уровни `n_o` и `n_e` по их угловым зависимостям в пропускании и отражении и одновременно добавляет более слабое независимое ограничение для `g33`. |
| C3 | x-срез, 20 мкм | 0° | 0°, 22,5°, 45°, 67,5° | пропускание | 4 | Определяет двупреломление `n_e - n_o`, делает `g33` непосредственно наблюдаемым и вместе с C1 разделяет вклады `g33` и `g11`. |
| C4 | x-срез, 20 мкм | 15°, 45° | 0°, 45° | пропускание и отражение для каждой пары угол/азимут | 8 | Уточняет `g33`, `n_o` и `n_e` по наклонному пропусканию и отражению и уменьшает оставшиеся корреляции между четырьмя подгоняемыми параметрами. |

Таким образом, полный набор данных содержит 29 матриц: 17 матриц пропускания и 12 матриц отражения.

Одного набора C1 недостаточно. При распространении вдоль оптической оси `n_e` и `g33` в этой модели вообще не влияют на результат, а `n_o` проявляется лишь очень слабо через относительный вес внутренне отраженных пучков. C3 обеспечивает второй срез кристалла, восстанавливающий чувствительность ко всем четырем параметрам. Затем C2 и C4 улучшают определение общего абсолютного уровня `n_o` и `n_e` через амплитуды Френеля при наклонном падении и внутренние углы преломления. Наклон образца z-среза не может заменить образец x-среза, поскольку закон Снеллиуса ограничивает внутренний угол распространения примерно 40,4° для кварца.

Толщина 20 мкм выбрана намеренно. Она удерживает линейную фазовую задержку меньше одной волны и исключает множество локальных минимумов, которые создала бы двупреломляющая пластина толщиной 1 мм. Пластина толщиной 1,0 мм используется только в C1, где нормальное распространение вдоль оптической оси не создает линейной фазовой задержки, а более длинный путь увеличивает оптическое вращение.

## 4. Порядок измерений

1. Откалибровать поляриметр на 632,8 нм в каналах пропускания и отражения, включая темновые сигналы, опорное пропускание, нуль угла, нуль азимута образца и соглашение о системе координат Стокса для отраженного пучка. Знаки внедиагональных элементов матрицы Мюллера следует проверить с помощью известного фазового элемента или ротатора.
2. Установить пластину z-среза толщиной 1,0 мм и измерить матрицу Мюллера пропускания C1 при нормальном падении и азимуте 0°.
3. Установить пластину z-среза толщиной 20 мкм. Для каждого из четырех углов падения измерить матрицы пропускания и отражения при азимутах 0° и 45°.
4. Установить пластину x-среза толщиной 20 мкм. При нормальном падении измерить матрицы пропускания для азимутов 0°, 22,5°, 45° и 67,5°.
5. С той же пластиной x-среза измерить матрицы пропускания и отражения для всех четырех сочетаний угла падения 15° или 45° и азимута 0° или 45°.
6. Сохранить каждую матрицу вместе с длиной волны, срезом, независимо измеренной толщиной, углом падения, азимутом, типом наблюдения, хиральностью и соглашением о поляризационной системе координат. Программный контракт требует по одной матрице для каждой перечисленной конфигурации. Физические повторы можно усреднять, однако сами повторы и их разброс также следует сохранять для оценки погрешности.

## 5. Обработка данных и решение обратной задачи

Для каждой измеренной матрицы `M` требуется конечное положительное значение `m00`; все элементы делятся на `m00`, после чего ставший тождественно равным единице нормированный элемент `m00 = 1` исключается из невязки. Каждая конфигурация дает 15 безразмерных невязок, поэтому 29 конфигураций образуют 435 компонент вектора невязки. Таким образом, абсолютная мощность источника, коэффициент усиления детектора и абсолютное пропускание не подгоняются.

До подгонки следует проверить физическую реализуемость матриц по собственным значениям матрицы когерентности Клауде. Также следует проверить индекс деполяризации и члены дихроизма. Синтетические матрицы пропускания почти не деполяризуют; отражение деполяризует значительно сильнее, поскольку поверхностно отраженный и внутренне отраженный пучки сравнимой интенсивности складываются некогерентно. Поэтому замкнутое обращение матрицы Мюллера–Джонса пригодно только как диагностический метод для пропускания или источник начальных значений, но не как окончательный результат для данных отражения.

Все 435 нормированных элементов следует одновременно, с одинаковым неявным весом, подгонять методом нелинейных наименьших квадратов к полной модели пластины Берремана. Материал в модели одноосный, с главной диэлектрической проницаемостью `diag(n_o², n_o², n_e²)` и двумя независимыми компонентами `Rho`. В основном тесте используется метод Левенберга–Марквардта, координаты показателей преломления масштабируются на `10^-3`, координаты оптической активности — на `10^-5`, допускается интервал ±0,05 около каждого начального показателя преломления и ±`5 × 10^-4` около каждого начального значения оптической активности, а число итераций ограничено 400.

Для точного численного замкнутого цикла синтетические истинные значения равны:

| Параметр | Синтетическое значение |
|---|---:|
| `n_o` | 1,542606 |
| `n_e` | 1,551651 |
| `g11` | `5,9 × 10^-5` |
| `g33` | `-10,1 × 10^-5` |

Начальные значения теста завышают `n_o` на 0,2%, `n_e` на 0,1%, а обе компоненты оптической активности отклоняют от истины на 30%. Для безошибочных данных, сгенерированных той же моделью, требуются относительная ошибка менее `10^-11` для каждого подогнанного параметра, полная сумма квадратов невязок менее `10^-18` и максимальная невязка нормированного элемента матрицы Мюллера менее `10^-9`. Эти пороги являются порогами численного регрессионного теста, а не лабораторными требованиями. Для реальных измерений нужны невязки с весами по погрешностям, оценки повторяемости, доверительные интервалы параметров и допуски, полученные из шума откалиброванного прибора.

## 6. Обязательное предупреждение о соглашении для параметров

Величины, называемые в тесте `g11` и `g33`, являются бианизотропными магнитоэлектрическими параметрами `Rho` движка Берремана в материальном соотношении `D = εE + ρH`. Они не являются непосредственно кристаллографическим тензором гирации в соглашении `D = εE + i(G × E)`, хотя для обеих величин часто используются одинаковые обозначения.

В соглашении движка вращение в C1 для z-среза определяется поперечной компонентой `g11` и подчиняется формуле `2π g11 d / λ`; синтетические константы дают примерно 33,57°/мм. Интерпретация приведенных констант кварца в кристаллографическом соглашении дает примерно 18,62°/мм, что близко к измеренной вращательной способности реального кварца. Следовательно, в лабораторной подгонке необходимо указывать, в каком соглашении оцениваются параметры, и выполнять явное проверенное преобразование перед сравнением подогнанных значений `Rho` с опубликованными значениями кристаллографической гирации. Без такого преобразования нельзя ожидать, что эксперимент с реальным кварцем воспроизведет синтетические значения `g11` и `g33` как физические литературные константы.
