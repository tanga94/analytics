import investpy

search_results = investpy.search_quotes(text='msci',
                                        products=['stocks'],
                                        countries=['united states'],
                                        n_results=10)
