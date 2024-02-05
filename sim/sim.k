size:5
g:(size;size)#25?1.0
flow:(size;size)#1
order>,/+1_-1_+1_-1_g
msort:{
    s
}
update:{
    e: x[0]
    f: x[1]
    o: x[2]
    n: o[0]

}
{#x[2]>0}{}/(g;flow;order)
