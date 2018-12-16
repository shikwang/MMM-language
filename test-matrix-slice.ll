; ModuleID = 'MMM'
source_filename = "MMM"

%matrix_t = type { double*, i32, i32 }

@fmt = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@fmt.1 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmt.2 = private unnamed_addr constant [4 x i8] c"%g\0A\00"

declare i32 @printf(i8*, ...)

declare i32 @open(i8*, i32, ...)

declare i32 @read(i32, double*, i32, ...)

declare i32 @creat(i8*, i32, ...)

declare i32 @write(i32, i8*, i32, ...)

declare i32 @close(i32, ...)

define i32 @main() {
entry:
  %a = alloca double
  %m1 = alloca %matrix_t*
  %res = alloca %matrix_t*
  %malloccall = tail call i8* @malloc(i32 mul (i32 ptrtoint (double* getelementptr (double, double* null, i32 1) to i32), i32 6))
  %system_mat = bitcast i8* %malloccall to double*
  %element_ptr = getelementptr double, double* %system_mat, i32 0
  store double 1.100000e+00, double* %element_ptr
  %element_ptr1 = getelementptr double, double* %system_mat, i32 1
  store double 2.200000e+00, double* %element_ptr1
  %element_ptr2 = getelementptr double, double* %system_mat, i32 2
  store double 3.300000e+00, double* %element_ptr2
  %element_ptr3 = getelementptr double, double* %system_mat, i32 3
  store double 4.400000e+00, double* %element_ptr3
  %element_ptr4 = getelementptr double, double* %system_mat, i32 4
  store double 5.500000e+00, double* %element_ptr4
  %element_ptr5 = getelementptr double, double* %system_mat, i32 5
  store double 6.600000e+00, double* %element_ptr5
  %malloccall6 = tail call i8* @malloc(i32 ptrtoint (%matrix_t* getelementptr (%matrix_t, %matrix_t* null, i32 1) to i32))
  %m = bitcast i8* %malloccall6 to %matrix_t*
  %m_mat = getelementptr inbounds %matrix_t, %matrix_t* %m, i32 0, i32 0
  store double* %system_mat, double** %m_mat
  %m_r = getelementptr inbounds %matrix_t, %matrix_t* %m, i32 0, i32 1
  store i32 2, i32* %m_r
  %m_c = getelementptr inbounds %matrix_t, %matrix_t* %m, i32 0, i32 2
  store i32 3, i32* %m_c
  store %matrix_t* %m, %matrix_t** %m1
  %malloccall7 = tail call i8* @malloc(i32 mul (i32 ptrtoint (double* getelementptr (double, double* null, i32 1) to i32), i32 2))
  %system_mat8 = bitcast i8* %malloccall7 to double*
  %element_ptr9 = getelementptr double, double* %system_mat8, i32 0
  store double 0.000000e+00, double* %element_ptr9
  %element_ptr10 = getelementptr double, double* %system_mat8, i32 1
  store double 0.000000e+00, double* %element_ptr10
  %malloccall11 = tail call i8* @malloc(i32 ptrtoint (%matrix_t* getelementptr (%matrix_t, %matrix_t* null, i32 1) to i32))
  %m12 = bitcast i8* %malloccall11 to %matrix_t*
  %m_mat13 = getelementptr inbounds %matrix_t, %matrix_t* %m12, i32 0, i32 0
  store double* %system_mat8, double** %m_mat13
  %m_r14 = getelementptr inbounds %matrix_t, %matrix_t* %m12, i32 0, i32 1
  store i32 2, i32* %m_r14
  %m_c15 = getelementptr inbounds %matrix_t, %matrix_t* %m12, i32 0, i32 2
  store i32 1, i32* %m_c15
  store %matrix_t* %m12, %matrix_t** %res
  %m116 = load %matrix_t*, %matrix_t** %m1
  %m_mat17 = getelementptr inbounds %matrix_t, %matrix_t* %m116, i32 0, i32 0
  %mat = load double*, double** %m_mat17
  %malloccall18 = tail call i8* @malloc(i32 mul (i32 ptrtoint (double* getelementptr (double, double* null, i32 1) to i32), i32 2))
  %system_mat19 = bitcast i8* %malloccall18 to double*
  %element_ptr20 = getelementptr double, double* %system_mat19, i32 0
  store double 0.000000e+00, double* %element_ptr20
  %element_ptr21 = getelementptr double, double* %system_mat19, i32 1
  store double 0.000000e+00, double* %element_ptr21
  %malloccall22 = tail call i8* @malloc(i32 ptrtoint (%matrix_t* getelementptr (%matrix_t, %matrix_t* null, i32 1) to i32))
  %m23 = bitcast i8* %malloccall22 to %matrix_t*
  %m_mat24 = getelementptr inbounds %matrix_t, %matrix_t* %m23, i32 0, i32 0
  store double* %system_mat19, double** %m_mat24
  %m_r25 = getelementptr inbounds %matrix_t, %matrix_t* %m23, i32 0, i32 1
  store i32 2, i32* %m_r25
  %m_c26 = getelementptr inbounds %matrix_t, %matrix_t* %m23, i32 0, i32 2
  store i32 1, i32* %m_c26
  %m_mat27 = getelementptr inbounds %matrix_t, %matrix_t* %m23, i32 0, i32 0
  %mat28 = load double*, double** %m_mat27
  %element_ptr_ptr = getelementptr double, double* %mat, i32 1
  %element_ptr29 = load double, double* %element_ptr_ptr
  %res_ptr_ptr = getelementptr double, double* %mat28, i32 0
  store double %element_ptr29, double* %res_ptr_ptr
  %element_ptr_ptr30 = getelementptr double, double* %mat, i32 4
  %element_ptr31 = load double, double* %element_ptr_ptr30
  %res_ptr_ptr32 = getelementptr double, double* %mat28, i32 1
  store double %element_ptr31, double* %res_ptr_ptr32
  store %matrix_t* %m23, %matrix_t** %res
  %res33 = load %matrix_t*, %matrix_t** %res
  %m_mat34 = getelementptr inbounds %matrix_t, %matrix_t* %res33, i32 0, i32 0
  %mat35 = load double*, double** %m_mat34
  %m_r36 = getelementptr inbounds %matrix_t, %matrix_t* %res33, i32 0, i32 1
  %r_mat = load i32, i32* %m_r36
  %m_c37 = getelementptr inbounds %matrix_t, %matrix_t* %res33, i32 0, i32 2
  %c_mat = load i32, i32* %m_c37
  %tmp = mul i32 0, %c_mat
  %index = add i32 0, %tmp
  %element_ptr_ptr38 = getelementptr double, double* %mat35, i32 %index
  %element_ptr39 = load double, double* %element_ptr_ptr38
  store double %element_ptr39, double* %a
  %a40 = load double, double* %a
  %printf = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @fmt.2, i32 0, i32 0), double %a40)
  ret i32 0
}

declare noalias i8* @malloc(i32)
