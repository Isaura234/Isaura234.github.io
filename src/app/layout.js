import { Montserrat } from 'next/font/google'
import './globals.css'
import Navigation from './components/navigation'
import Sidebar from './components/sidebar'

const montserrat = Montserrat({ subsets: ['latin'] })

export const metadata = {
  title: 'Isaura Portfolio',
}

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body className={montserrat.className}>
        <Navigation/>
        <div className="container mx-auto px-4 lg:grid grid-cols-12 gap-x-4 pt-10 lg:pt-20">
          <Sidebar/>
          <div className="col-span-10 col-start-3 mt-8 lg:mt-0">
            {children}
          </div>
        </div>
      </body>
    </html>
  )
}
