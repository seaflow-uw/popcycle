from sqlalchemy import create_engine, MetaData, Table


class PopcycleDb(object):
    def __init__(self, filename):
        self.__engine = create_engine('sqlite:///{}'.format(filename))
        self.__meta = MetaData()
        self.__meta.reflect(bind=self.__engine)
        if 'evt' not in self.__meta.tables:
            raise IOError("{db} does not contain an evt table".format(
                db=filename))

    def store_evt(self, particles, is_evt=False):
        if is_evt:
            table = self.__meta.tables['evt']
        else:
            table = self.__meta.tables['opp']

        self.__engine.execute(table.insert(), particles)


if __name__ == "__main__":
    # Assumes that popcycle.db has been created by
    #     sqlite3 popcycle.db < sql/popcycle.sql
    db = PopcycleDb('popcycle.db')

    # Load the provided test data from the EVT and store it into the db
    import load
    particles = load.load_evt('data/37.evt', 'testcruise', 'testfile')
    db.store_evt(particles, is_evt=True)