create database test;

use test;

create table t(a1 int, a2 int);
create table s(a1 int, a3 int); 
create table u(a1 int, a4 int);
create table v(a3 int, a4 int); 

insert into t(a1,a2) values(10,11);
insert into s(a1,a3) values(10,15);
insert into u(a1,a4) values(10,16);
insert into v(a3,a4) values(15,16);


create join index jdx6 on t(a2), s(a3), u, v where (t.a1 = s.a1) and  (s.a1 = u.a1) and  (v.a3 = s.a3) and  (v.a4 = u.a4);

select t.a2, s.a3  from t,s,u,v where  (t.a1 = s.a1) and  (s.a1 = u.a1) and  (v.a3 = s.a3) and  (v.a4 = u.a4);
