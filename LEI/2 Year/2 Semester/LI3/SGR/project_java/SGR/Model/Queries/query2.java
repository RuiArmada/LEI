package Model.Queries;


    //private Boolean isValidMonth(int month) {
    //    boolean res = true;
    //    if(month >= 12)
    //        res = false;
    //    return res;
    //}

    //private Boolean isValidYear(int year) {
    //    boolean res = true;
    //    if(year >= 2021 || year <= 2000)
    //        res = false;
    //    return res;
    //}

    //public Table query2(Date init, Date end) {
    //    int counterRev = 0;
    //    int counterUse = 0;
    //    List<User> list1 = new ArrayList<>();
    //    List<Review> list2 = new ArrayList<>();
    //    int yearI = init.getYear();
    //    int yearE = end.getYear();
    //    int monthI = init.getMonth();
    //    int monthE = end.getMonth(); 

    //    if(isValidYear(yearI) && isValidYear(yearE))
    //        if(isValidMonth(monthI) && isValidMonth(monthE)) {

    //            list2 = this.reviews.stream()
    //                                .filter(r -> r.getDate().isBefore(end) && r.getDate().isAfter(init))
    //                                .collect(Collectors.toList());
    //            counterRev = list2.size();

    //            list2 = this.reviews.stream()
    //                                .filter(r -> r.getUser_id())
    //                                .collect(Collectors.toList());
    //            counterUse = list1.size();
            
    //        }
    //    System.out.println("Number of Users: " + counterUse);
    //    System.out.println("Number of Reviews: " + counterRev);
    //}
    // 
    

