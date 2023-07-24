import sys
import math


def get_movies_from_args():
    movies_file = open(sys.argv[3], "r")
    try:
        return map(int, movies_file)
    except IOError:
        print("movies file was invalid")
        return []


def get_ratings_from_args(movies):
    rating_file = open(sys.argv[1] + "./ratings.dat", "r")
    print("gathering ratings data...")
    ratings = []  # userID, movieID
    for line in rating_file:
        rating = tuple(int(x) for x in line.split("::")[:2])
        if rating[1] in movies:
            ratings += [rating]
    print("done.")
    return ratings


def calculate_v_users_movies(ratings, movies):
    print("calculating v{}{}...")
    v = {}
    for rating in ratings:
        if rating[0] not in v.keys():
            v[rating[0]] = {}
            for movie in movies:
                v[rating[0]][movie] = 0
        v[rating[0]][rating[1]] = 1
    print("done.")
    return v


def calculate_n(ratings):
    print("\tcalculating n[..]...")
    n = {}
    for rating in ratings:
        if rating[0] not in n.keys():
            n[rating[0]] = 0
        n[rating[0]] += 1
    print("\tdone.")
    return n


def calculate_m(ratings):
    print("\tcalculating m[..]...")
    m = {}
    for rating in ratings:
        if rating[1] not in m.keys():
            m[rating[1]] = 0
        m[rating[1]] += 1
    print("\tdone.")
    return m


def list_of_movies(ratings):
    return list(set(movie for user, movie in ratings))


def print_table_of_watches(v):
    for user in v.keys():
        print(str(user) + ":\t" + reduce(lambda acc, cur: acc + "" + ("X" if cur else " "),
                                         [v[user][x] for x in v[user]], ""))


def print_probabilities(p, movies):
    for m1 in movies:
        print(str(p[m1]) + str([p[m1, m2] for m2 in movies]))


def align_users(v, ratings):
    n = calculate_n(ratings)
    users_to_remove = [user for user in n.keys() if n[user] < 20]
    aligned = False
    if len(users_to_remove) > 0:
        print("there are " + str(len(users_to_remove)) + " users to remove")
        ratings = [(user, movie) for user, movie in ratings if user not in users_to_remove]
        for user in users_to_remove:
            v.pop(user)
        aligned = True
    return v, ratings, aligned


def align_movies(v, ratings):
    m = calculate_m(ratings)
    movies_to_remove = [movie for movie in m.keys() if m[movie] < 10]
    aligned = False
    if len(movies_to_remove) > 0:
        print("there are " + str(len(movies_to_remove)) + " movies to remove")
        ratings = [(user, movie) for user, movie in ratings if movie not in movies_to_remove]
        for user in v.keys():
            for movie in movies_to_remove:
                v[user].pop(movie)
        aligned = True
    return v, ratings, aligned


def align_data(v, ratings):
    print("aligning data...")
    def align_data_from_users(v, ratings):
        v, ratings, changed_users = align_users(v, ratings)
        if changed_users:
            v, ratings = align_data_from_movies(v, ratings)
        return v, ratings

    def align_data_from_movies(v, ratings):
        v, ratings, changed_movies = align_movies(v, ratings)
        if changed_movies:
            v, ratings = align_data_from_users(v, ratings)
        return v, ratings

    v, ratings, _ = align_users(v, ratings)
    print("done.")
    return align_data_from_movies(v, ratings)


def calculate_probabilities(v, n, movies):
    print("calculating probabilities...")
    k = float(len(movies))
    p = {}

    for movie in movies:
        p[movie] = 2.0 / k
        for user in v:
            watches = v[user]
            p[movie] += 2.0*watches[movie]/n[user]
        p[movie] /= float(len(v.keys()) + 1)

    for m1 in movies:
        for m2 in movies:
            p[m1, m2] = 2.0 / (k * (k-1))
            for user in v:
                watches = v[user]
                p[m1, m2] += 2.0*(watches[m1]*watches[m2]) / float(n[user]*(n[user]-1))
            p[m1, m2] /= float(len(v.keys()) + 1)

    print("done")
    return p


def cost(C, p):
    if len(C) == 0:
        return 0
    c = C[0]
    C_ = C[1:]
    sum = 0.0
    if len(c) == 1:
        sum = math.log(1/p[c[0]])
    else:
        for i in range(0, len(c)):
            for j in range(i+1, len(c)):
                sum += math.log(1/p[c[i], c[j]])
        sum /= float(len(c)-1)
    return sum + cost(C_, p)


def clustering1(p, movies):
    c = {}
    for m1 in movies:
        for m2 in movies:
            c[m1, m2] = '+' if p[m1, m2] > p[m1]*p[m2] else '-'

    def cc_pivot(E, M):
        if len(M) == 0:
            return []
        m = M[0]  # pick random
        C = [m]
        M_ = []
        for m_ in M:
            if m_!=m:
                if E[m, m_] == '+':
                    C += [m_]
                elif E[m, m_] == '-':
                    M_ += [m_]
                else:
                    raise ValueError
        return [C] + cc_pivot(E, M_)
    return cc_pivot(c, movies)


def clustering2(p, movies):
    pass


# moviecluster <data set folder> <1/2> <movie subset file path>

movies = get_movies_from_args()

ratings = get_ratings_from_args(movies)

v = calculate_v_users_movies(ratings, movies)

v, ratings = align_data(v, ratings)

# print_table_of_watches(v)

p = calculate_probabilities(v, calculate_n(ratings), list_of_movies(ratings))

# print_probabilities(p, list_of_movies(ratings))

try:
    clustering_algorithm = [clustering1, clustering2][int(sys.argv[2])-1]
except IndexError or ValueError:
    def clustering_algorithm(p):
        print("invalid algorithm selected")
        return []

C = (clustering_algorithm(p, list_of_movies(ratings)))

print ("cost: " + str(cost(C, p)))

print ("clustering: " + str(C))

# print (cost([[m] for m in list_of_movies(ratings)], p))  # just to test if cost and clustering_algorithm kinda work
