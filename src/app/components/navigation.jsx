'use client'

import { usePathname } from 'next/navigation'
import Link from 'next/link'

const Navigation = () => {
  const pathname = usePathname();
  const linkClasses = 'underline-offset-4 text-white text-lg font-medium px-3 py-1 rounded-full hover:bg-cyan-500 transition';

  return (
    <div className="bg-gradient-to-r from-cyan-700 to-indigo-600 px-4 py-6 shadow-md">
      <div className="container mx-auto px-4">
        <nav>
          <ul className="flex gap-x-12">
            <li>
              <Link className={`${linkClasses} ${pathname === '/' && 'bg-cyan-600'}`} href="/">
                Home
              </Link>
            </li>
            <li>
              <Link
                className={`${linkClasses} ${pathname === '/cv' && 'bg-cyan-600'}`}
                href="/cv"
              >
                CV
              </Link>
            </li>
            <li>
              <Link
                className={`${linkClasses} ${pathname === '/portfolio' && 'bg-cyan-600'}`}
                href="/portfolio"
              >
                Portfolio
              </Link>
            </li>
          </ul>
        </nav>
      </div>
    </div>
  );
};

export default Navigation;
