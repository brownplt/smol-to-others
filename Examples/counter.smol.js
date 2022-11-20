let f = ((ctr) => {
    return (() => {
        ctr = (ctr + 1);
        return ctr
    })
})(0);
console.log(f());
console.log(f());