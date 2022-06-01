using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Where2Go.Controllers
{
    public class DirectionController : Controller
    {
        public IActionResult Index()
        {
            return View();
        }
    }
}
