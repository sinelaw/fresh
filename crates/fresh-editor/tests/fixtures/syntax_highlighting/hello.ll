; A small LLVM IR module.
source_filename = "saxpy.c"

define float @multiply_add(float %a, float %x, float %y) {
entry:
  %product = fmul fast float %a, %x
  %result = fadd float %product, %y
  ret float %result, !dbg !12
}

!12 = !{!"result location", i32 7}
