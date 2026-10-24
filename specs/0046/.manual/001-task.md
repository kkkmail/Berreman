# Spec 0046 — task (verbatim operator request, 2026-10-24)

We are going to work on improving code in Berreman repo. The folder for placing consecutive NNN MDs
is `C:\GitHub\Berreman\specs\0046\.manual\`. Here is what you need to do.

1. Make this repo use centralized nuget versions, like in STL repo:
   `C:\GitHub\Softellect\Directory.Build.targets` and up all package versions here to the same versions
   as used in STL.
2. Same as in STL, remove DEBUG build.
3. Introduce version numbering system, similar to what is used in STL, but just simpler. Let's stick
   with the version system that STL is using for the slow ring and use something like that but just
   for the whole app. The Optical Constructor (Launcher) should show this version somewhere at the
   bottom of the form.
4. Each element (except full mirrors) has the "ability" to specify the reflected ray. I think that it
   was "attached" to the right click context menu. But now the menu is gone, and I do not see any
   functionality to specify the reflected ray for the sample AND attach elements to the reflected ray
   even if the element support both T and R.
5. Subsequently, the experiments "offer" to capture both T and R but then draws only T, perhaps
   because there is no R on the element and no detector to capture R. What to capture must be
   determined dynamically based on what detectors are in the system: if it is only a T detector, then
   the experiments must offer only T and so on and so forth. The experiments show the table below
   experiment data => the table does not fit the window vertically BUT the window does not have scroll
   bars.
6. Rendering of transmitted ray is incorrect when the sample is rotated on R2 or R3. The ray must hit
   the surface of the sample, then propagate through it using Snell's law and then when it exits the
   sample, then it becomes parallel to the original beam. So, that's 3 segments with 1 3 parallel to
   each other. But now there are two AND the second one rotates when R2 or R3 are changes. Perhaps the
   second one is what is supposed to be inside the sample. Obviously, this must be fixed for all
   elements. So there is no separate logic / rendering.

Fix the issues.
