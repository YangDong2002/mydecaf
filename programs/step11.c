
int add(int a, int b) {
    return a + b;
}

int ff(int *a) {
    *a = 114514;
}

int g() {
    int a;
    a = 4;
    a = (int) &a;
    *&a = 4;
    return a;
}

int main() {
    int *a = (int *) 114514;
    int b = 1919810;
    int *c = &b;
    int **d = &c;
    int ***e = &d;
    ***e = 333;
    *&e = &d;

    return add((int) a, b);
}
