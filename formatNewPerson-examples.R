tp1 <- formatNewPerson(relation = "proband", ped.id = "A1", sx = 0)
tp1 <- formatNewPerson(relation = "mother", tmp.ped = tp1)
tp1 <- formatNewPerson(relation = "father", tmp.ped = tp1)

tp1 <- formatNewPerson(relation = "partner", tmp.ped = tp1)
tp1 <- formatNewPerson(relation = "daughter", tmp.ped = tp1, f.id = 4)
tp1 <- formatNewPerson(relation = "son", tmp.ped = tp1, f.id = 4)

tp1 <- formatNewPerson(relation = "rel.partner", tmp.ped = tp1, partner.of = 5)
tp1 <- formatNewPerson(relation = "niece", tmp.ped = tp1, m.id = 5, f.id = 7)

tp1 <- formatNewPerson(relation = "rel.partner", tmp.ped = tp1, partner.of = 6)
tp1 <- formatNewPerson(relation = "nephew", tmp.ped = tp1, m.id = 9, f.id = 6)

tp1 <- formatNewPerson(relation = "sister", tmp.ped = tp1)
tp1 <- formatNewPerson(relation = "brother", tmp.ped = tp1)

tp1 <- formatNewPerson(relation = "grandmother", tmp.ped = tp1, m.or.p.side = "m")
tp1 <- formatNewPerson(relation = "grandfather", tmp.ped = tp1, m.or.p.side = "m")

tp1 <- formatNewPerson(relation = "aunt", tmp.ped = tp1, m.or.p.side = "m")
tp1 <- formatNewPerson(relation = "uncle", tmp.ped = tp1, m.or.p.side = "m")

tp1 <- formatNewPerson(relation = "rel.partner", tmp.ped = tp1, partner.of = 15)
tp1 <- formatNewPerson(relation = "cousin", tmp.ped = tp1, m.id = 15, f.id = 17, sx = 1)

tp1 <- formatNewPerson(relation = "grandmother", tmp.ped = tp1, m.or.p.side = "p")
tp1 <- formatNewPerson(relation = "grandfather", tmp.ped = tp1, m.or.p.side = "p")

tp1 <- formatNewPerson(relation = "aunt", tmp.ped = tp1, m.or.p.side = "p")
tp1 <- formatNewPerson(relation = "uncle", tmp.ped = tp1, m.or.p.side = "p")

tp1 <- formatNewPerson(relation = "rel.partner", tmp.ped = tp1, partner.of = 22)
tp1 <- formatNewPerson(relation = "cousin", tmp.ped = tp1, m.id = 23, f.id = 22, sx = 0)
