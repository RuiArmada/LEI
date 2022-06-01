using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using Where2Go.Data;
using Where2Go.Models;

namespace Where2Go.Controllers
{
    public class PointsOfInterestController : Controller
    {

        private readonly ApplicationDbContext _db;

        public PointsOfInterestController(ApplicationDbContext db)
        {
            _db = db;
        }

        public IActionResult Index()
        {
            IEnumerable<PointsOfInterest> pointOfInterestList = _db.PointsOfInterests;

            return View(pointOfInterestList);
        }

        [HttpPost]
        public IActionResult GetMap(string TypePOI)
        {
            TempData["TypePOI"] = TypePOI;

            return RedirectToAction("Index", "Direction");
        }
    }
}
