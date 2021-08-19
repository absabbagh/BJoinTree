create database test2;

use test2;

create table t(t1 int, t2 int, t3 int, t4 int);
create table s(s1 int, s2 int, s3 int, s4 int);
create table u(u1 int, u2 int, u3 int);
create table v(v1 int, v2 int, v3 int);
create table w(w1 int, w2 int, w3 int, w4 int);

create join index jdx4 on t,s,u,v,w where (t.t2 = s.s2) and (s.s4 = u.u3) and (u.u2 = v.v3) and (v.v1 = w.w1); 

insert into t(t1, t2, t3, t4) values(101,102,103,10);
insert into s(s1, s2, s3, s4) values(10,102, 1323, 1133);
insert into u(u1, u2, u3) values(4410,44102,1133);
insert into v(v1, v2, v3) values(22310,22102, 44102);
insert into w(w1, w2, w3, w4) values(22310,10442, 132333, 113332);


select t.t1, w.w2 from t,s,u,v,w where (t.t2 = s.s2) and (s.s4 = u.u3) and (u.u2 = v.v3) and (v.v1 = w.w1); 


