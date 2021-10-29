# SGR - C Version

### 🎯 About the Project

Implementation of a recommendation management system in C progamming Language.

# How to Run
* `cd SGR/src` to go to the sorce code file
* `make` to run the makefile and build all essencial files
* `make run` to run the application

# How to Use

### Load the files
* `load_sgr()` -> By ommition
* `load_sgr("path/to/users","path/to/business","path/to/reviewsi")`

### Run the Queries (Examples)
* `show(businesses_started_by_letter(d,''))`
* `show(business_info(d,"B_ID"))`
* `show(businesses_reviewed(d,"U_ID"))`
* `show(businesses_with_stars_and_city(d,"stars","city"))`
* `show(top_businesses_by_city(d,"top"))`
* `show(international_users(d))`
* `show(top_businesses_with_category(d,"top","category"))`
* `show(reviews_with_word(d,"WORD"))`

