# Experiments Required to Replicate the Primary Test of `MuellerInverseTests.fs`

The primary test is `the full C1-C4 measurement set recovers every material
constant from a perturbed start`. It solves the inverse problem for a
transparent, optically active, anisotropic, homogeneous uniaxial crystal —
α-quartz at a single wavelength — and asserts that all four material constants
are recovered from the measured Mueller matrices. Below is the complete
experiment one would have to perform to replicate it.

## 1. Sample and ground truth

- Material: right-handed α-quartz (crystal class 32; a uniaxial gyrotropic,
  non-absorbing crystal).
- Wavelength: 632.8 nm (He-Ne laser line), fixed for the whole experiment; no
  dispersion is involved.
- Ground-truth constants at this wavelength:
  - ordinary refractive index n_o = 1.542606 (Ghosh 1999 Sellmeier);
  - extraordinary refractive index n_e = 1.551651 (birefringence
    n_e − n_o = 0.009045);
  - gyration component perpendicular to the optic axis g11 = 5.9e-5;
  - gyration component along the optic axis g33 = −10.1e-5.
- The "measured" data are full 4×4 Mueller matrices computed by the Berreman
  4×4 forward solver for a plane-parallel plate, including multiple internal
  reflections (3 reflections, i.e. the transmitted/reflected signal is the
  incoherent sum of the direct beam and the beams that have bounced twice
  inside the plate). The data are noiseless.

## 2. Measurement configurations (C1–C4)

All measurements are complete Mueller polarimetry of the plate; the normalized
Mueller matrix (all 16 elements divided by m00) is recorded, and the 15
elements other than m00 enter the fit. Two sample cuts and two plate
thicknesses are used:

- z-cut: optic axis along the surface normal;
- x-cut: optic axis lying in the surface plane;
- thick plate: 1.0 mm (used only where there is no linear retardance);
- thin plate: 20 µm = 0.286 waves at 632.8 nm (sub-wave linear retardance
  keeps the chi-squared surface smooth and unimodal; at 1 mm a
  retardance-bearing configuration would carry 14.29 waves and the residual
  would fragment into local minima roughly half a fringe apart).

Plate thickness is KNOWN and is not fitted; it is a design variable only.

- C1 (1 measurement) — z-cut, 1.0 mm, normal incidence, azimuth 0°,
  transmission. The wave travels along the optic axis: no linear
  birefringence, the plate is a pure optical rotator (rotation ≈ 18.62°).
  This configuration measures g11 cleanly and is completely blind to g33 and
  n_e; n_o enters only weakly through the multiple-reflection weight.
- C2 (16 measurements) — z-cut, 20 µm, oblique-incidence sweep: incidence
  angles 10°, 30°, 50°, 70° × sample azimuths 0°, 45° × {transmission,
  reflection}. Supplies the Fresnel/angle information that pins the absolute
  refractive indices and lets g33 enter weakly (Snell's law caps the internal
  angle at arcsin(1/n_o) = 40.41°).
- C3 (4 measurements) — x-cut, 20 µm, normal incidence, azimuths 0°, 22.5°,
  45°, 67.5°, transmission. The optic axis lies in the transverse plane, so
  linear birefringence is maximal and g33 finally becomes observable. This is
  the hard measurement: the large linear retardance buries the much smaller
  optical activity.
- C4 (8 measurements) — x-cut, 20 µm, incidence angles 15°, 45° × azimuths
  0°, 45° × {transmission, reflection}. A second independent look at g33 plus
  extra index/angle leverage.

Total: 29 Mueller matrices, hence a residual vector of 29 × 15 = 435
normalized element differences.

The reason two crystal cuts are mandatory: tilting a z-cut plate can never
bring the propagation direction more than ~40.4° off the optic axis (Snell
refraction cap), so the axial gyration component g33 stays unreachable no
matter how many angles are measured on that one sample. The ablation part of
the test suite demonstrates this quantitatively: from C1 alone the Jacobian
columns for n_e and g33 are exactly zero, n_o is constrained ~700× more weakly
than g11, and the problem is singular (condition number > 1e12); adding the
x-cut configuration C3 restores observability of all four parameters.

## 3. Data reduction / fit

- Invert the four parameters p = (n_o, n_e, g11, g33) by nonlinear least
  squares (Levenberg–Marquardt), minimizing the sum of squares of the 435
  residuals — differences between the measured and modelled normalized Mueller
  elements.
- Work in a dimensionless scaled parameter space centred on the start guess:
  one scaled unit = 1e-3 in refractive index and 1e-5 in gyration. (The
  optimizer differentiates with a fixed absolute step of 1e-6, so physical
  units must be rescaled for the Jacobian to be meaningful.)
- Start guess deliberately wrong: n_o × 1.002, n_e × 1.001 (which moves the
  birefringence by −17 %), g11 × 1.3, g33 × 1.3. The index perturbation cannot
  be made larger in a single-wavelength fit: linear retardance enters through
  cos/sin of 2π(n_e − n_o)d/λ, so a start more than about half a fringe away
  lands in a different basin of attraction.
- Box bounds ±50 scaled units (±0.05 in index, ±5e-4 in gyration); at most 400
  iterations; parameter-step convergence tolerance epsX = 1e-12.
- Any parameter vector the forward solver cannot evaluate is mapped to a large
  finite penalty residual (1e3 per entry) of the correct length, so the fit
  never aborts mid-search.

## 4. Acceptance criteria (what counts as replication)

- The fit moves away from the start (at least one iteration).
- Each of the four constants is recovered to a relative error below 1e-11.
- The final chi-squared is below 1e-18, and the worst normalized Mueller
  element residual at the solution is below 1e-9 — i.e. the recovered
  parameters reproduce the measurements, not merely sit near the truth.
- Guard against a vacuous pass: the start guess itself must lie outside the
  acceptance band (max relative start error > 1e-3).

## 5. Supporting checks performed alongside the primary test

- C1 reproduces the closed-form rotatory power 2π·g11·d/λ to machine precision
  (relative deviation ~2e-16), with zero linear retardance and zero dichroism.
- C1 is exactly insensitive to g33: doubling it, flipping its sign, or zeroing
  it changes nothing in the measured matrix.
- Every measured Mueller matrix is physically realizable (no negative Cloude
  coherency eigenvalue beyond round-off) and essentially non-depolarizing:
  worst transmission depolarization 1 − DI < 0.05; overall (reflection from
  the thick plate genuinely depolarizes) < 0.5.
- Identifiability at the solution: every Jacobian column norm > 1e-6, the
  Jacobian condition number finite and < 5e3, and all parameter correlations
  below 0.5 except the n_o/n_e pair, which is physically correlated at
  ≈0.99999 (the data constrain the difference n_e − n_o ~170× better than the
  common level) and is bounded at 0.999999.

---
---

# Эксперименты, необходимые для воспроизведения основного теста `MuellerInverseTests.fs`

Основной тест — `the full C1-C4 measurement set recovers every material
constant from a perturbed start`. Он решает обратную задачу для прозрачного,
оптически активного, анизотропного, однородного одноосного кристалла —
α-кварца на одной длине волны — и проверяет, что все четыре константы
материала восстанавливаются из измеренных матриц Мюллера. Ниже приведено
полное описание эксперимента, который нужно выполнить для его воспроизведения.

## 1. Образец и эталонные значения

- Материал: правовращающий α-кварц (кристаллический класс 32; одноосный
  гиротропный непоглощающий кристалл).
- Длина волны: 632,8 нм (линия He-Ne лазера), фиксированная для всего
  эксперимента; дисперсия не рассматривается.
- Эталонные константы на этой длине волны:
  - обыкновенный показатель преломления n_o = 1,542606 (Сельмейер по Ghosh
    1999);
  - необыкновенный показатель преломления n_e = 1,551651 (двулучепреломление
    n_e − n_o = 0,009045);
  - компонента гирации перпендикулярно оптической оси g11 = 5,9e-5;
  - компонента гирации вдоль оптической оси g33 = −10,1e-5.
- «Измеренные» данные — полные матрицы Мюллера 4×4, вычисленные прямым
  решателем Берремана 4×4 для плоскопараллельной пластины с учётом многократных
  внутренних отражений (3 отражения, т.е. прошедший/отражённый сигнал —
  некогерентная сумма прямого луча и лучей, дважды отразившихся внутри
  пластины). Данные без шума.

## 2. Конфигурации измерений (C1–C4)

Все измерения — полная мюллеровская поляриметрия пластины; регистрируется
нормированная матрица Мюллера (все 16 элементов делятся на m00), и 15
элементов, кроме m00, входят в подгонку. Используются два среза образца и две
толщины пластины:

- z-срез: оптическая ось вдоль нормали к поверхности;
- x-срез: оптическая ось лежит в плоскости поверхности;
- толстая пластина: 1,0 мм (используется только там, где нет линейного
  двулучепреломления);
- тонкая пластина: 20 мкм = 0,286 длины волны при 632,8 нм (субволновое линейное
  двулучепреломление сохраняет поверхность хи-квадрат гладкой и одномодальной;
  при 1 мм конфигурация с двулучепреломлением содержала бы 14,29 длин волн, и
  невязка распалась бы на локальные минимумы с шагом примерно в половину
  полосы).

Толщина пластины ИЗВЕСТНА и не подгоняется; это лишь проектная переменная.

- C1 (1 измерение) — z-срез, 1,0 мм, нормальное падение, азимут 0°,
  пропускание. Волна распространяется вдоль оптической оси: линейного
  двулучепреломления нет, пластина является чистым оптическим вращателем
  (вращение ≈ 18,62°). Эта конфигурация чисто измеряет g11 и полностью слепа к
  g33 и n_e; n_o входит лишь слабо через вес многократных отражений.
- C2 (16 измерений) — z-срез, 20 мкм, сканирование по углам падения: углы
  падения 10°, 30°, 50°, 70° × азимуты образца 0°, 45° × {пропускание,
  отражение}. Даёт френелевскую/угловую информацию, фиксирующую абсолютные
  показатели преломления, и позволяет g33 войти слабо (закон Снелла ограничивает
  внутренний угол величиной arcsin(1/n_o) = 40,41°).
- C3 (4 измерения) — x-срез, 20 мкм, нормальное падение, азимуты 0°, 22,5°,
  45°, 67,5°, пропускание. Оптическая ось лежит в поперечной плоскости, поэтому
  линейное двулучепреломление максимально, и g33 наконец становится
  наблюдаемой. Это трудное измерение: большое линейное двулучепреломление
  скрывает гораздо более слабую оптическую активность.
- C4 (8 измерений) — x-срез, 20 мкм, углы падения 15°, 45° × азимуты 0°, 45° ×
  {пропускание, отражение}. Второй независимый взгляд на g33 плюс дополнительный
  рычаг для показателей преломления и углов.

Итого: 29 матриц Мюллера, т.е. вектор невязок из 29 × 15 = 435 разностей
нормированных элементов.

Причина обязательности двух срезов кристалла: наклоном z-среза невозможно
отклонить направление распространения более чем на ~40,4° от оптической оси
(ограничение рефракции по Снеллу), поэтому аксиальная компонента гирации g33
остаётся недостижимой, сколько бы углов ни измерялось на одном этом образце.
Абляционная часть тестового набора демонстрирует это количественно: по одной
только C1 столбцы якобиана для n_e и g33 в точности равны нулю, n_o ограничен
примерно в 700 раз слабее, чем g11, а задача сингулярна (число обусловленности
> 1e12); добавление конфигурации C3 с x-срезом восстанавливает наблюдаемость
всех четырёх параметров.

## 3. Обработка данных / подгонка

- Инвертировать четыре параметра p = (n_o, n_e, g11, g33) методом нелинейных
  наименьших квадратов (Левенберг–Марквардт), минимизируя сумму квадратов 435
  невязок — разностей между измеренными и модельными нормированными элементами
  Мюллера.
- Работать в безразмерном масштабированном пространстве параметров с центром в
  начальном приближении: одна масштабная единица = 1e-3 для показателя
  преломления и 1e-5 для гирации. (Оптимизатор дифференцирует с фиксированным
  абсолютным шагом 1e-6, поэтому физические единицы необходимо
  перемасштабировать, чтобы якобиан имел смысл.)
- Начальное приближение намеренно неверное: n_o × 1,002, n_e × 1,001 (что
  сдвигает двулучепреломление на −17 %), g11 × 1,3, g33 × 1,3. Возмущение
  показателей преломления нельзя сделать большим в одноволновой подгонке:
  линейное двулучепреломление входит через cos/sin от 2π(n_e − n_o)d/λ, поэтому
  старт далее примерно половины полосы попадает в другой бассейн притяжения.
- Границы ящика ±50 масштабных единиц (±0,05 для показателя преломления,
  ±5e-4 для гирации); не более 400 итераций; допуск сходимости по шагу
  параметров epsX = 1e-12.
- Любой вектор параметров, который прямой решатель не может вычислить,
  отображается в большую конечную штрафную невязку (1e3 на элемент) правильной
  длины, чтобы подгонка никогда не прерывалась в середине поиска.

## 4. Критерии приёмки (что считается воспроизведением)

- Подгонка действительно ушла от начальной точки (не менее одной итерации).
- Каждая из четырёх констант восстановлена с относительной ошибкой менее 1e-11.
- Итоговый хи-квадрат менее 1e-18, а наихудшая невязка нормированного элемента
  Мюллера в решении менее 1e-9 — то есть восстановленные параметры
  воспроизводят измерения, а не просто находятся рядом с истиной.
- Защита от пустого прохождения: само начальное приближение должно лежать вне
  полосы приёмки (максимальная относительная ошибка старта > 1e-3).

## 5. Сопутствующие проверки, выполняемые вместе с основным тестом

- C1 воспроизводит аналитическую формулу вращательной способности 2π·g11·d/λ с
  машинной точностью (относительное отклонение ~2e-16), при нулевом линейном
  двулучепреломлении и нулевом дихроизме.
- C1 в точности нечувствительна к g33: удвоение, смена знака или обнуление
  ничего не меняют в измеренной матрице.
- Каждая измеренная матрица Мюллера физически реализуема (нет отрицательных
  собственных значений когерентной матрицы Клауда сверх ошибок округления) и
  практически не деполяризует: наихудшая деполяризация в пропускании
  1 − DI < 0,05; в целом (отражение от толстой пластины действительно
  деполяризует) < 0,5.
- Идентифицируемость в решении: норма каждого столбца якобиана > 1e-6, число
  обусловленности якобиана конечно и < 5e3, а все корреляции параметров ниже
  0,5, кроме пары n_o/n_e, которая физически коррелирована на уровне ≈0,99999
  (данные ограничивают разность n_e − n_o примерно в 170 раз лучше, чем общий
  уровень) и ограничена сверху значением 0,999999.
