using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using Where2Go.Data;
using Where2Go.Models;

namespace Where2Go.Controllers
{
    public class StartPageController : Controller
    {
        private readonly ApplicationDbContext _db;

        public StartPageController(ApplicationDbContext db)
        {
            _db = db;
        }

        public IActionResult Index()
        {
            IEnumerable<City> citiesList = _db.Cities;

            return View(citiesList);
        }

        [HttpPost]
        public IActionResult GetCityAndPOI(string CityName, string TypePOI)
        {
            TempData["CityName"] = CityName;
            TempData["TypePOI"] = TypePOI;

            return RedirectToAction("Index", "PointsOfInterest");
        }
    }
}
