import Image from 'next/image'
import profilePic from '../../../public/me.png'
import { Location, Email, LinkedIn, Github } from './icons';

const Sidebar = () => {
  const listClasses = 'text-sm font-medium text-gray-600 flex items-center gap-x-2 transition';

  return (
    <aside className="col-span-2">
      <Image
        src={profilePic}
        width={175}
        height={175}
        alt="Picture of the author"
        className="rounded-full mb-8 aspect-square object-cover border border-cyan-600"
      />
      <h2 className="text-lg font-medium">Isaura Gutiérrez</h2>
      <h3 className="font-medium mb-4 text-sm">Economist | Statistician</h3>
      <ul className="flex flex-col gap-y-2">
        <li><a target="_blank" className={`${listClasses} hover:text-cyan-600`} href="https://www.google.com/maps/place/Costa+Rica/data=!4m2!3m1!1s0x8f92e56221acc925:0x6254f72535819a2b?sa=X&ved=2ahUKEwjLgu2AjraDAxVHfDABHZrQDsoQ8gF6BAgbEAA"><Location/> Costa Rica</a></li>
        <li><a target="_blank" className={`${listClasses} hover:text-cyan-600`} href="emailto:isaguti.var26@gmail.com"><Email/> Email</a></li>
        <li><a target="_blank" className={`${listClasses} hover:text-[#0077B5]`} href="https://www.linkedin.com/in/isauragutierrez"><LinkedIn/> LinkedIn</a></li>
        <li><a target="_blank" className={`${listClasses} hover:text-black`} href="https://github.com/Isaura234"><Github/> Github</a></li>
      </ul>
    </aside>
  );
};

export default Sidebar;
